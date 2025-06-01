const std = @import("std");
const parser_module = @import("parser.zig");

const Token = parser_module.Token;
const TokenKind = parser_module.TokenKind;
pub const Tokenizer = struct {
    string_source: []const u8,
    int_pos: usize,
    main_alloc: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !*Tokenizer {
        const self = try allocator.create(Tokenizer);
        self.* = Tokenizer{
            .string_source = source,
            .int_pos = 0,
            .main_alloc = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Tokenizer) void {
        self.main_alloc.destroy(self);
    }

    fn peek(self: *Tokenizer, offset: usize) ?u8 {
        const pos = self.int_pos + offset;
        if (pos >= self.string_source.len) {
            return null;
        }
        return self.string_source[pos];
    }

    fn current(self: *Tokenizer) ?u8 {
        return self.peek(0);
    }

    fn next_char(self: *Tokenizer) ?u8 {
        return self.peek(1);
    }

    pub fn tokenize(self: *Tokenizer) !?*Token {

        while (self.int_pos < self.string_source.len) {
            const current_char = self.string_source[self.int_pos];
            if (std.ascii.isWhitespace(current_char)) {
                self.int_pos += 1;
                continue;
            }
            break; 
        }

        if (self.int_pos >= self.string_source.len) {
            return null;
        }

        const head_token_ptr: *Token = try self.main_alloc.create(Token);
        head_token_ptr.* = Token{
            .kind = TokenKind.Reserved,
            .string_index = 0,
            .length = 0,
            .prev = null,
            .next = null,
        };
        var current_token: *Token = head_token_ptr; 

        while (self.int_pos < self.string_source.len) {
            const current_char: u8 = self.string_source[self.int_pos];
            if (std.ascii.isWhitespace(current_char)) {
                self.int_pos += 1;
                continue;
            }

            if (current_char == '[') {
                const start_pos = self.int_pos;
                var balance: usize = 1;
                var temp_pos = self.int_pos + 1;
                var found_match = false;
                while(temp_pos < self.string_source.len) {
                    if (self.string_source[temp_pos] == '[') {
                        balance += 1;
                    } else if (self.string_source[temp_pos] == ']') {
                        balance -= 1;
                        if (balance == 0) {
                            const length = temp_pos - start_pos + 1;
                            const container_token: *Token = try self.main_alloc.create(Token);
                            container_token.* = Token{
                                .kind = TokenKind.Container,
                                .string_index = start_pos,
                                .length = length,
                                .prev = current_token,
                                .next = null,
                            };

                            current_token.next = container_token;
                            current_token = container_token;
                            self.int_pos = temp_pos + 1;
                            found_match = true;
                            break;
                        }
                    }
                    temp_pos += 1;
                }
                if (found_match) {
                    continue;
                }
            }

            const nfunc_keyword = "nfunc";
            if (std.mem.startsWith(u8, self.string_source[self.int_pos..], nfunc_keyword)) {
                if (self.string_source.len == self.int_pos + nfunc_keyword.len or
                !std.ascii.isAlphanumeric(self.string_source[self.int_pos + nfunc_keyword.len]))
                {
                    const nfunc_token: *Token = try self.main_alloc.create(Token);
                    nfunc_token.* = Token{
                        .kind = TokenKind.KeywordNtFunc,
                        .string_index = self.int_pos,
                        .length = nfunc_keyword.len,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = nfunc_token;
                    current_token = nfunc_token;
                    self.int_pos += nfunc_keyword.len;
                    continue;
                }
            }

            const keyword_int = "int";
            if (std.mem.startsWith(u8, self.string_source[self.int_pos..], keyword_int)) {
                if (self.string_source.len == self.int_pos + keyword_int.len or
                !std.ascii.isAlphanumeric(self.string_source[self.int_pos + keyword_int.len]))
                {
                    const int_token: *Token = try self.main_alloc.create(Token);
                    int_token.* = Token{
                        .kind = TokenKind.KeywordInt,
                        .string_index = self.int_pos,
                        .length = keyword_int.len,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = int_token;
                    current_token = int_token;
                    self.int_pos += keyword_int.len;
                    continue;
                }
            }

            if (std.ascii.isDigit(current_char)) {
                const start_pos = self.int_pos;
                while (self.int_pos < self.string_source.len and std.ascii.isDigit(self.string_source[self.int_pos])) {
                    self.int_pos += 1;
                }
                const length = self.int_pos - start_pos;
                const number_token: *Token = try self.main_alloc.create(Token);
                number_token.* = Token{
                    .kind = TokenKind.Number,
                    .string_index = start_pos,
                    .length = length,
                    .prev = current_token,
                    .next = null,
                };
                current_token.next = number_token;
                current_token = number_token;
                continue;
            }

            if (std.ascii.isAlphabetic(current_char) or current_char == '_') {
                const start_pos = self.int_pos;
                while (self.int_pos < self.string_source.len and (std.ascii.isAlphanumeric(self.string_source[self.int_pos]) or self.string_source[self.int_pos] == '_')) {
                    self.int_pos += 1;
                }
                const length = self.int_pos - start_pos;
                const identifier_token: *Token = try self.main_alloc.create(Token);
                identifier_token.* = Token{
                    .kind = TokenKind.Identifier,
                    .string_index = start_pos,
                    .length = length,
                    .prev = current_token,
                    .next = null,
                };
                current_token.next = identifier_token;
                current_token = identifier_token;
                continue;
            }

            // +-*/()<>,=;.
        
            switch (current_char) {
                '+' => {

                    if (self.next_char()) |next| {
                        if (next == '+') {
                            const op_token: *Token = try self.main_alloc.create(Token);
                            op_token.* = Token{
                                .kind = TokenKind.Increment,
                                .string_index = self.int_pos,
                                .length = 2,
                                .prev = current_token,
                                .next = null,
                            };

                            current_token.next = op_token;
                            current_token = op_token;
                            self.int_pos += 2;
                            continue;
                        }
                    }

                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.Plus,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                '-' => {
                    
                    if (self.next_char()) |next| {
                        if (next == '>') {
                            const op_token: *Token = try self.main_alloc.create(Token);
                            op_token.* = Token{
                                .kind = TokenKind.Arrow,
                                .string_index = self.int_pos,
                                .length = 2,
                                .prev = current_token,
                                .next = null,
                            };
                            current_token.next = op_token;
                            current_token = op_token;
                            self.int_pos += 2;
                            continue;
                        }
                       else if (next == '-') {
                            const op_token: *Token = try self.main_alloc.create(Token);
                            op_token.* = Token{
                                .kind = TokenKind.Decrement,
                                .string_index = self.int_pos,
                                .length = 2,
                                .prev = current_token,
                                .next = null,
                            };

                            current_token.next = op_token;
                            current_token = op_token;
                            self.int_pos += 2;
                            continue;
                        }
                    }

                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.Minus,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                '*' => {
                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.Multiply,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                '/' => {
                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.Divide,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },

                '(' => {
                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.LeftParen,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                ')' => {
                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.RightParen,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                ';' => {
                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.Semicolon,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                ',' => {
                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.Comma,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                '[' => {
                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.LeftBracket,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                ']' => {
                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.RightBracket,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                '{' => {
                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.LeftBrace,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                '}' => {
                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.RightBrace,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },
                '=' => {
                    if (self.next_char()) |next| {
                        if (next == '=') {
                            const op_token: *Token = try self.main_alloc.create(Token);
                            op_token.* = Token{
                                .kind = TokenKind.Equal,
                                .string_index = self.int_pos,
                                .length = 2,
                                .prev = current_token,
                                .next = null,
                            };
                            current_token.next = op_token;
                            current_token = op_token;
                            self.int_pos += 2;
                            continue;
                        }
                    }

                    const op_token: *Token = try self.main_alloc.create(Token);
                    op_token.* = Token{
                        .kind = TokenKind.Assign,
                        .string_index = self.int_pos,
                        .length = 1,
                        .prev = current_token,
                        .next = null,
                    };
                    current_token.next = op_token;
                    current_token = op_token;

                    self.int_pos += 1;
                    continue;
                },

                else => {
                    std.debug.print("Warning: Unknown character '{c}' at position {d}. Skipping.\n", .{ current_char, self.int_pos });
                    self.int_pos += 1;
                }
            }
        }

        const eof_token: *Token = try self.main_alloc.create(Token);
        eof_token.* = Token{
            .kind = TokenKind.EndOfFile,
            .string_index = self.int_pos,
            .length = 0,
            .prev = current_token,
            .next = null,
        };
        current_token.next = eof_token;

        if (head_token_ptr.next == eof_token and head_token_ptr.kind == TokenKind.Reserved) {
          
        }
        const actual_first_token = head_token_ptr.next;
        self.main_alloc.destroy(head_token_ptr);

        return actual_first_token;
    }


};