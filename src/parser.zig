const std = @import("std");

pub const NodeKind = enum  {
        Add,
        Sub,
        Mul,
        Div,
        Eq,
        Ne,
        Lt,
        Gt,
        Le,
        Ge,
        Assign,
        Block,
        Function,
        For,
        If,
        While,
    };

    pub const TokenKind = enum  {
       Reserved,
       Number,
       Identifier,
       String,
       Operator,
       Keyword,
       Comment,
       Whitespace,
       EndOfFile,
       Invalid,      
    };

    pub const Token = struct {
    kind: TokenKind,
    string_index: usize,
    length: usize,
    prev: ?*Token,
    next: ?*Token,
    };

    pub const AstNodeKind = enum {
        NumberLiteral,
        Indentifier,
    };

    pub const AstNode = struct {
        kind: AstNodeKind,
        value: ?i64 = null,
        identifier_value: ?[]const u8 = null,
        allocator: *std.mem.Allocator,

        pub fn deinit(self: *AstNode) void {
           if (self.kind == .Indentifier) {
                if (self.identifier_value) |id_val| {
                    self.allocator.free(id_val);
                }
            }
            self.allocator.destroy(self);
        }
    };

pub const Parser = struct {

    current_token: ?*Token,
    allocator: *std.mem.Allocator,
    source: []const u8,

    pub fn init(allocator: *std.mem.Allocator, first_token: ?*Token, source: []const u8) Parser {
        return Parser{
            .current_token = first_token,
            .allocator = allocator,
            .source = source,
        };
    }
    
    fn advance(self: *Parser) void {
        if (self.current_token) |token| {
            self.current_token = token.next;
        }
    }

    fn parse_number(self: *Parser) !*AstNode {
        const tok = self.current_token orelse return error.UnexpectedToken;
        
        if (tok.kind == TokenKind.Number) {
            const num_str = self.source[tok.string_index..tok.string_index + tok.length];
            const num = std.fmt.parseInt(i64, num_str, 10) catch return error.InvalidNumber;

            const node = self.allocator.create(AstNode) catch return error.OutOfMemory;
            node.* = AstNode{
                .kind = AstNodeKind.NumberLiteral,
                .value = num,
                .allocator = self.allocator,
            };
            self.advance();
            return node;
        }
        return error.UnexpectedToken;
    }

    fn parse_identifier(self: *Parser) !*AstNode {
        const tok = self.current_token orelse return error.UnexpectedToken;
        
        if (tok.kind == TokenKind.Identifier) {
            const id_str = self.source[tok.string_index..tok.string_index + tok.length];
            const id = self.allocator.dupe(u8,id_str) catch return error.OutOfMemory;
            const node = self.allocator.create(AstNode) catch return error.OutOfMemory;
            node.* = AstNode{
                .kind = AstNodeKind.Indentifier,
                .identifier_value = id,
                .allocator = self.allocator,
            };
            self.advance();
            return node;
        }
        return error.UnexpectedToken;
    }

    pub fn parse_expression(self: *Parser) !*AstNode {

        const tok = self.current_token orelse return error.UnexpectedEof;

        switch (tok.kind) {
            .Number => return self.parse_number(),
            .Identifier => return self.parse_identifier(),
            .EndOfFile => return error.UnexpectedEof,
            else => return error.UnexpectedToken,
        }
    }

    pub fn evalute(self: *Parser,node: *AstNode) !EvaluatedValue {
        switch (node.kind) {
            .NumberLiteral => {
                return EvaluatedValue{
                    .Number = node.value.?
                };
            },
            .Indentifier => {
                const allocator =  self.allocator;
                const copied_id = allocator.dupe(u8, node.identifier_value orelse return error.InvalidNode) catch return error.OutOfMemory;
                return EvaluatedValue{
                    .Identifier = copied_id,
                };
            }

        }
    }

};

pub fn free_token_list(allocator: *std.mem.Allocator, head: *Token) void {
    var current: ?*Token = head;
    while (current) |tok_ptr| {
        const next = tok_ptr.next;
        allocator.destroy(tok_ptr);
        current = next;
    }
}

pub const EvaluatedValue = union(enum) {
    Number: i64,
    Identifier: []const u8,

    pub fn deinit(self: EvaluatedValue, allocator: *std.mem.Allocator) void {
        switch (self) {
            .Identifier => |id_slice| {
                allocator.free(id_slice);
            },
            else => {},
        }
    }
};