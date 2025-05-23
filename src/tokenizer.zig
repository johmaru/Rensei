const std = @import("std");
const parser_module = @import("parser.zig");

const Token = parser_module.Token;
const TokenKind = parser_module.TokenKind;
pub const Tokenizer = struct {
    string_source: []const u8,
    int_pos: usize,
    main_alloc: *std.mem.Allocator,

    pub fn init(allocator: *std.mem.Allocator, source: []const u8) !*Tokenizer {
        const self = try allocator.create(Tokenizer);
        self.* = Tokenizer{
            .string_source = source,
            .int_pos = 0,
            .main_alloc = allocator,
        };
        return self;
    }

    pub fn tokenize(self: *Tokenizer) !?*Token {

        while (self.int_pos < self.string_source.len) {
            const current_char = self.string_source[self.int_pos];
            if (current_char == ' ' or current_char == '\n' or current_char == '\t') {
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
            if (current_char == ' ' or current_char == '\n' or current_char == '\t') {
                self.int_pos += 1;
                continue;
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

            if (std.ascii.isAlphabetic(current_char)) {
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

            std.debug.print("Warning: Unknown character '{c}' at position {d}. Skipping.\n", .{ current_char, self.int_pos });
            self.int_pos += 1;
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