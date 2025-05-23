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
       Plus,
       Minus,
       Multiply,
       Divide,
       LeftParen,
       RightParen,
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
        Identifier,
        BinaryOperation,
    };

    pub const BinaryOperator = enum {
        Add,
        Subtract,
        Multiply,
        Divide,
    };

    pub const AstNode = struct {
        kind: AstNodeKind,
        value: ?i64 = null,
        identifier_value: ?[]const u8 = null,
        operator: ?BinaryOperator = null,
        left: ?*AstNode = null,
        right: ?*AstNode = null,
        allocator: *std.mem.Allocator,

        pub fn deinit(self: *AstNode) void {
            if (self.left) |left_node| {
                left_node.deinit();
            }
            if (self.right) |right_node| {
                right_node.deinit();
            }

            if (self.kind == .Identifier) {
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

    fn parse_number(self: *Parser) ParseError!*AstNode {
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

    fn parse_identifier(self: *Parser) ParseError!*AstNode {
        const tok = self.current_token orelse return error.UnexpectedToken;
        
        if (tok.kind == TokenKind.Identifier) {
            const id_str = self.source[tok.string_index..tok.string_index + tok.length];
            const id = self.allocator.dupe(u8,id_str) catch return error.OutOfMemory;
            const node = self.allocator.create(AstNode) catch return error.OutOfMemory;
            node.* = AstNode{
                .kind = AstNodeKind.Identifier,
                .identifier_value = id,
                .allocator = self.allocator,
            };
            self.advance();
            return node;
        }
        return error.UnexpectedToken;
    }

    pub fn parse_expression(self: *Parser) ParseError!*AstNode {
        return self.parse_additive();
    }

    fn parse_additive(self: *Parser) ParseError!*AstNode {
        var left = try self.parse_multiplicative();

        while (self.current_token) |tok| {
            const op = switch (tok.kind) {
                .Plus => BinaryOperator.Add,
                .Minus => BinaryOperator.Subtract,
                else => break,
            };

            self.advance();
            const right = try self.parse_multiplicative();

            const node = try self.allocator.create(AstNode);
            node.* = AstNode{
                .kind = AstNodeKind.BinaryOperation,
                .operator = op,
                .left = left,
                .right = right,
                .allocator = self.allocator,
            };
            left = node;
        }

        return left;
    }

    const ParseError = error{
        UnexpectedToken,
        UnexpectedEof,
        MissingCloseParen,
        InvalidNumber,
        DivideByZero,
        TypeMismatch,
        InvalidNode,
        OutOfMemory,
    };

    fn parse_primary(self: *Parser) ParseError!*AstNode {

        const tok = self.current_token orelse return error.UnexpectedEof;

        switch (tok.kind) {
            .Number => return self.parse_number(),
            .Identifier => return self.parse_identifier(),
            .LeftParen => {
                self.advance();
                const expr = try self.parse_expression();

                const close_tok = self.current_token orelse return error.UnexpectedEof;
                if (close_tok.kind != TokenKind.RightParen) {
                    return error.MissingCloseParen;
                }
                self.advance();
                return expr;
            },
            .EndOfFile => return error.UnexpectedEof,
            else => return error.UnexpectedToken,
        }
    }

    fn parse_multiplicative(self: *Parser) ParseError!*AstNode {
        var left = try self.parse_primary();

        while (self.current_token) |tok| {
            const op = switch (tok.kind) {
                .Multiply => BinaryOperator.Multiply,
                .Divide => BinaryOperator.Divide,
                else => break,
            };

            self.advance();
            const right = try self.parse_primary();

            const node = try self.allocator.create(AstNode);
            node.* = AstNode{
                .kind = AstNodeKind.BinaryOperation,
                .operator = op,
                .left = left,
                .right = right,
                .allocator = self.allocator,
            };
            left = node;
        }

        return left;
    }

    pub fn evalute(self: *Parser,node: *AstNode) ParseError!EvaluatedValue {
        switch (node.kind) {
            .NumberLiteral => {
                return EvaluatedValue{
                    .Number = node.value.?
                };
            },
            .Identifier => {
                const allocator =  self.allocator;
                const copied_id = allocator.dupe(u8, node.identifier_value orelse return error.InvalidNode) catch return error.OutOfMemory;
                return EvaluatedValue{
                    .Identifier = copied_id,
                };
            },
            .BinaryOperation => {
                const left_val = try self.evalute(node.left.?);
                defer left_val.deinit(self.allocator);
                const right_val = try self.evalute(node.right.?);
                defer right_val.deinit(self.allocator);

                if (left_val == .Number and right_val == .Number) {
                    const result = switch (node.operator.?) {
                        .Add => left_val.Number + right_val.Number,
                        .Subtract => left_val.Number - right_val.Number,
                        .Multiply => left_val.Number * right_val.Number,
                        .Divide => if (right_val.Number == 0) return error.DivideByZero else @divTrunc(left_val.Number, right_val.Number)
                    };
                    return EvaluatedValue{
                        .Number = result,
                    };
                }
                return error.TypeMismatch;
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