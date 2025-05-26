const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const Symbol = @import("symbol.zig");

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
       Increment,
       Minus,
       Decrement,
       Multiply,
       Divide,
       LeftParen,
       RightParen,
       Semicolon,
       LeftBrace,
       RightBrace,
       LeftBracket,
       RightBracket,
       KeywordNtFunc,
       KeywordRtFunc,
       KeywordInt,
       Container,
       Arrow,
       Block,
       Comma,
       Assign,
       Equal,
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
        UnaryOperation,
        FunctionDefinition,
        VariableDeclaration,
        StatementList,
        SingleContainer,
        ContainerChain,
    };

    pub const BinaryOperator = enum {
        Add,
        Subtract,
        Multiply,
        Divide,
    };

    pub const UnaryOperator = enum {
        PreIncrement,
        PreDecrement,
    };

    pub const AstNode = struct {
        kind: AstNodeKind,
        value: ?i64 = null,
        identifier_value: ?[]const u8 = null,
        operator: ?BinaryOperator = null,
        left: ?*AstNode = null,
        right: ?*AstNode = null,
        unary_operator: ?UnaryOperator = null,
        operand: ?*AstNode = null,
        allocator: std.mem.Allocator,

        function_name: ?[]const u8 = null,
        function_params: ?*AstNode = null,
        function_body: ?*AstNode = null,

        variable_name: ?[]const u8 = null,
        variable_type: ?[]const u8 = null,
        initializer: ?*AstNode = null,

        container_keyword: ?[]const u8 = null,
        container_identifier: ?[]const u8 = null,
        container_arguments: ?*AstNode = null,
        container_body: ?*AstNode = null,

        statements: ?std.ArrayList(*AstNode) = null,

        pub fn deinit(self: *AstNode) void {
          
            if (self.identifier_value) |s| self.allocator.free(s);
            if (self.function_name) |s| self.allocator.free(s);
            if (self.variable_name) |s| self.allocator.free(s);
            if (self.variable_type) |s| self.allocator.free(s);
            if (self.container_keyword) |s| self.allocator.free(s);
            if (self.container_identifier) |s| self.allocator.free(s);

            if (self.left) |node| node.deinit();
            if (self.right) |node| node.deinit();
            if (self.operand) |node| node.deinit();
            if (self.function_params) |node| node.deinit();
            if (self.function_body) |node| node.deinit();
        
            if (self.initializer) |node| node.deinit();
            if (self.container_arguments) |node| node.deinit();
            if (self.container_body) |node| node.deinit();

            if (self.statements) |list| {
                for (list.items) |item_node| {
                    item_node.deinit();
                }
                list.deinit(); 
            }
            self.allocator.destroy(self);
        }
    };

pub const Parser = struct {

    const parser_module = @import("parser.zig");

    current_token: ?*Token,
    allocator: std.mem.Allocator,
    source: []const u8,

    symbol_table: *Symbol.SymbolTable,

    pub fn init(allocator: std.mem.Allocator, first_token: ?*Token, source: []const u8,symbol_table: *Symbol.SymbolTable) Parser {
        return Parser{
            .current_token = first_token,
            .allocator = allocator,
            .source = source,
            .symbol_table = symbol_table,
        };
    }
    
    fn advance(self: *Parser) void {
        if (self.current_token) |token| {
            self.current_token = token.next;
        }
    }

    fn expect_token(self: *Parser, expected_kind: TokenKind) ParseError!void {
        const tok = self.current_token orelse return error.UnexpectedEof;
        if (tok.kind != expected_kind) {
            return error.UnexpectedToken;
        }
        self.advance();
    }

    fn parse_parameter_list_and_register_symbols(self: *Parser) ParseError!*AstNode {
        try self.expect_token(TokenKind.LeftParen);

        var params_ast_nodes = std.ArrayList(*AstNode).init(self.allocator);
        errdefer {
            if (params_ast_nodes.items.len > 0) {
            }
            params_ast_nodes.deinit();
        }

        var first_param = true;
        while (self.current_token.?.kind != TokenKind.RightParen and 
               self.current_token.?.kind != TokenKind.EndOfFile) {
            if (!first_param) {
                try self.expect_token(TokenKind.Comma);
            }
            first_param = false;

            const param_node = try self.parse_identifier();
            
            _ = try self.symbol_table.add_symbol(param_node.identifier_value.?, .Variable, .Int);

            try params_ast_nodes.append(param_node);
        }

        try self.expect_token(TokenKind.RightParen);

        const list_node = try self.allocator.create(AstNode);
        list_node.* = AstNode{
            .kind = AstNodeKind.StatementList,
            .allocator = self.allocator,
            .statements = params_ast_nodes,
        };
        return list_node;
    }

    fn parse_parameter_list(self: *Parser) ParseError!*AstNode {
        try self.expect_token(TokenKind.LeftParen);

        var params = std.ArrayList(*AstNode).init(self.allocator);
        errdefer {
            for (params.items) |param| {
                param.deinit();
            }
            params.deinit();
        }

        var first_param = true;
        while (self.current_token.?.kind != TokenKind.RightParen and 
               self.current_token.?.kind != TokenKind.EndOfFile) {
            if (!first_param) {
                try self.expect_token(TokenKind.Comma);
            }
            first_param = false;

            const param_node = try self.parse_identifier();
            try params.append(param_node);
        }

        try self.expect_token(TokenKind.RightParen);

        const list_node = try self.allocator.create(AstNode);
        list_node.* = AstNode{
            .kind = AstNodeKind.StatementList,
            .allocator = self.allocator,
            .statements = params,
        };
        return list_node;
    }

    fn parse_block_statement(self: *Parser) ParseError!*AstNode {
        try self.expect_token(TokenKind.LeftBrace);

        var statements = std.ArrayList(*AstNode).init(self.allocator);
        errdefer {
            for (statements.items) |stmt| {
                stmt.deinit();
            }
            statements.deinit();
        }

        try self.symbol_table.enter_scope();
        defer self.symbol_table.exit_scope();

        while (self.current_token.?.kind != TokenKind.RightBrace and
               self.current_token.?.kind != TokenKind.EndOfFile) {
                const stmt_node = try self.parse_statement();
                try statements.append(stmt_node);
               }

        try self.expect_token(TokenKind.RightBrace);
        const block_node = try self.allocator.create(AstNode);
        block_node.* = AstNode{
            .kind = AstNodeKind.StatementList,
            .allocator = self.allocator,
            .statements = statements,
        };
        return block_node;
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
        UnsupportedUnaryOperation,
        InvalidContainerFormat,
        InvalidContainerBlock,
        NotSupportedThisVersion,
    };

    fn parse_unary(self: *Parser) ParseError!*AstNode {
        if (self.current_token) |tok| {
            const unary_op_kind = switch (tok.kind) {
                .Increment => UnaryOperator.PreIncrement,
                .Decrement => UnaryOperator.PreDecrement,
                else => return self.parse_primary(),
            };

            self.advance();

            const operand_node = try self.parse_primary();

            const node = try self.allocator.create(AstNode);
            node.* = AstNode{
                .kind = AstNodeKind.UnaryOperation,
                .unary_operator = unary_op_kind,
                .operand = operand_node,
                .allocator = self.allocator,
            };
            return node;
        }
        return self.parse_primary();
    }

    pub fn parse_statement(self: *Parser) ParseError!*AstNode {
        const tok = self.current_token orelse return error.UnexpectedEof;

        switch (tok.kind) {
            .Block => {
                return error.NotSupportedThisVersion;
            },
            .Identifier => {
              const expr_node = try self.parse_expression();
              try self.expect_token(TokenKind.Semicolon);
              return expr_node;
            },
            .KeywordNtFunc => {
                self.advance();

                try self.expect_token(TokenKind.Semicolon);

                const func_name_tok = self.current_token orelse return error.UnexpectedEof;
                if (func_name_tok.kind != TokenKind.Identifier) {
                    return error.UnexpectedToken;
                }
                const func_name_str = self.source[func_name_tok.string_index..func_name_tok.string_index + func_name_tok.length];
                const func_name_copy = try self.allocator.dupe(u8, func_name_str);
                self.advance();

                try self.expect_token(TokenKind.Semicolon);

                _ = try self.symbol_table.add_symbol(func_name_copy, .Function, .Int);

                try self.symbol_table.enter_scope();
                defer self.symbol_table.exit_scope();

                const params_node = try self.parse_parameter_list_and_register_symbols();

                try self.expect_token(TokenKind.Semicolon);
            
                
                const body_node = try self.parse_block_statement();

                try self.expect_token(TokenKind.Semicolon);

                const func_node = try self.allocator.create(AstNode);
                func_node.* = AstNode{
                    .kind = AstNodeKind.FunctionDefinition,
                    .function_name = func_name_copy,
                    .function_params = params_node,
                    .function_body = body_node,
                    .allocator = self.allocator,
                };
                return func_node;
            },
            .KeywordInt => {
                self.advance();

                const var_name_tok = self.current_token orelse return error.UnexpectedEof;
                if (var_name_tok.kind != TokenKind.Identifier) {
                    return error.UnexpectedToken;
                }
                const var_name_str = self.source[var_name_tok.string_index..var_name_tok.string_index + var_name_tok.length];
                const var_name_copy = try self.allocator.dupe(u8, var_name_str);
                self.advance();

                const type_name_copy = try self.allocator.dupe(u8, "int");
                if (self.symbol_table.find_symbol(var_name_copy)) |existing_symbol| {
                   if (std.mem.eql(u8, existing_symbol.name, var_name_copy)) {
                        return error.TypeMismatch; 
                    }
                }
                errdefer self.allocator.free(type_name_copy);

                var intializer_node: ?*AstNode = null;
                if (self.current_token) |next_token| {
                    if (next_token.kind == TokenKind.Assign) {
                        self.advance();
                        intializer_node = self.parse_expression() catch |err| {
                            return err;
                        };
                    }
                }

                try self.expect_token(TokenKind.Semicolon);

                _ = try self.symbol_table.add_symbol(var_name_copy, .Variable, .Int);

                const var_node = try self.allocator.create(AstNode);
                var_node.* = AstNode{
                    .kind = AstNodeKind.VariableDeclaration,
                    .variable_name = var_name_copy,
                    .variable_type = type_name_copy,
                    .initializer = intializer_node,
                    .allocator = self.allocator,
                };
                return var_node;
            },
            else => {
                const expr_node = try self.parse_expression();

                try self.expect_token(TokenKind.Semicolon);
                return expr_node;
            },
        }
    }

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
            .Container => {
                return self.parse_container_chain();
            },
            .EndOfFile => return error.UnexpectedEof,
            else => return error.UnexpectedToken,
        }
    }

    fn parse_block_from_token(self: *Parser, block_token: *Token) ParseError!*AstNode {
        const block_content_str = self.source[block_token.string_index..block_token.string_index + block_token.length];

        var temp_tokenizer = try Tokenizer.init(self.allocator, block_content_str);
        defer temp_tokenizer.deinit();

        const head_token = try temp_tokenizer.tokenize();
        defer if (head_token) |tok| {
            parser_module.free_token_list(self.allocator, tok);
        };

        var sub_parser = Parser.init(self.allocator, head_token, block_content_str);

        sub_parser.symbol_table = self.symbol_table;
        try self.symbol_table.enter_scope();

        // test
        self.symbol_table.print();

        var statements = std.ArrayList(*AstNode).init(self.allocator);
        errdefer {
            for (statements.items) |stmt| {
                stmt.deinit();
            }
            statements.deinit();
            self.symbol_table.exit_scope();
        }

        while (sub_parser.current_token.?.kind != TokenKind.EndOfFile) {
            const stmt_node = try sub_parser.parse_statement();
            try statements.append(stmt_node);
        }

        self.symbol_table.exit_scope();

        const block_node = try self.allocator.create(AstNode);
        block_node.* = AstNode{
            .kind = AstNodeKind.StatementList,
            .allocator = self.allocator,
            .statements = statements,
        };
        return block_node;
        
    }

    fn parse_single_container_from_token(self: *Parser, container_token: *Token) ParseError!*AstNode {
        const full_container_str = self.source[container_token.string_index..container_token.string_index + container_token.length];

        if (full_container_str.len < 2 or full_container_str[0] != '[' or full_container_str[full_container_str.len - 1] != ']') {
            return error.InvalidContainerFormat;
        }
        const content_str = full_container_str[1..full_container_str.len - 1];

        var parts_iterator = std.mem.splitScalar(u8, content_str, ';');

        const keyword_str = std.mem.trim(u8, parts_iterator.next() orelse return error.InvalidContainerFormat, " \t\n\r");
        if (keyword_str.len == 0) {
            return error.InvalidContainerFormat;
        }

        const identifier_str = std.mem.trim(u8, parts_iterator.next() orelse return error.InvalidContainerFormat, " \t\n\r");
        if (identifier_str.len == 0) {
            return error.InvalidContainerFormat;
        }

        const args_str = std.mem.trim(u8, parts_iterator.next() orelse return error.InvalidContainerFormat, " \t\n\r");
        if (args_str.len == 0) {
            return error.InvalidContainerFormat;
        }
        const args_node = try self.allocator.create(AstNode);
        errdefer self.allocator.destroy(args_node);
        args_node.* = AstNode{
            .kind = AstNodeKind.Identifier,
            .identifier_value = try self.allocator.dupe(u8, args_str),
            .allocator = self.allocator,
        };

        const block_full_str = std.mem.trim(u8, parts_iterator.rest()," \t\n\r");
        if (block_full_str.len < 2 or block_full_str[0] != '{' or block_full_str[block_full_str.len - 1] != '}') {
            if (args_node.identifier_value) |id_val| self.allocator.free(id_val);
            self.allocator.destroy(args_node);
            return error.InvalidContainerBlock;
        }

        const block_inner_content = block_full_str[1 .. block_full_str.len - 1];

        var temp_tokenizer_for_container_block = try Tokenizer.init(self.allocator, block_inner_content);
        defer temp_tokenizer_for_container_block.deinit();
        const head_token_for_container_block = try temp_tokenizer_for_container_block.tokenize();
        defer if (head_token_for_container_block) |cb_tok| parser_module.free_token_list(self.allocator, cb_tok);

        var sub_parser_for_container_block = Parser.init(self.allocator, head_token_for_container_block, block_inner_content,self.symbol_table);
        sub_parser_for_container_block.symbol_table = self.symbol_table;

        try self.symbol_table.enter_scope();

        var container_statements = std.ArrayList(*AstNode).init(self.allocator);
        errdefer {
            for (container_statements.items) |stmt| {
                stmt.deinit();
            }
            container_statements.deinit();
            self.symbol_table.exit_scope();
        }

        while (sub_parser_for_container_block.current_token.?.kind != TokenKind.EndOfFile) {
            const stmt_node = try sub_parser_for_container_block.parse_statement();
            try container_statements.append(stmt_node);
        }

        self.symbol_table.exit_scope();

        const parsed_block_node = try self.allocator.create(AstNode);
        parsed_block_node.* = AstNode{
            .kind = AstNodeKind.StatementList,
            .allocator = self.allocator,
            .statements = container_statements,
        };

        const node = try self.allocator.create(AstNode);
         errdefer{
            if (args_node.identifier_value) |id_val| self.allocator.free(id_val);
            self.allocator.destroy(args_node);
        }
        
        node.* = AstNode{
            .kind = AstNodeKind.SingleContainer,
            .allocator = self.allocator,
            .container_keyword = try self.allocator.dupe(u8, keyword_str),
            .container_identifier = try self.allocator.dupe(u8, identifier_str),
            .container_arguments = args_node,
            .container_body = parsed_block_node,
        };
        return node;
    }

    fn parse_container_chain(self: *Parser) ParseError!*AstNode {
        const first_container_token = self.current_token orelse return error.UnexpectedEof;
        if (first_container_token.kind != TokenKind.Container) {
            return error.InvalidContainerFormat;
        }
        self.advance();

        var left_node = try self.parse_single_container_from_token(first_container_token);

        while (self.current_token) |tok| {
            if (tok.kind == TokenKind.Arrow) {
                self.advance();

                const next_container_token = self.current_token orelse return error.UnexpectedEof;
                if (next_container_token.kind != TokenKind.Container) {
                    left_node.deinit();
                    return error.InvalidContainerFormat;
                }
                self.advance();

                const right_node = try self.parse_single_container_from_token(next_container_token);
                errdefer right_node.deinit();

                const chain_node = try self.allocator.create(AstNode);
                chain_node.* = AstNode {
                    .kind = AstNodeKind.ContainerChain,
                    .allocator = self.allocator,
                    .left = left_node,
                    .right = right_node,
                };
                left_node = chain_node;
            } else {
                break;
            }
        }
        return left_node;
    }

    fn parse_multiplicative(self: *Parser) ParseError!*AstNode {
        var left = try self.parse_unary();

        while (self.current_token) |tok| {
            const op = switch (tok.kind) {
                .Multiply => BinaryOperator.Multiply,
                .Divide => BinaryOperator.Divide,
                else => break,
            };

            self.advance();
            const right = try self.parse_unary();

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
                if (node.value) |val| {
                    return EvaluatedValue{
                        .Number = val,
                    };
                } else {
                     return error.InvalidNode;
                }
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
            },
            .UnaryOperation => {
                const operand_val = try self.evalute(node.operand.?);
                defer operand_val.deinit(self.allocator);

                if (operand_val == .Number) {
                    const result = switch (node.unary_operator.?) {
                        .PreIncrement => operand_val.Number + 1,
                        .PreDecrement => operand_val.Number - 1,
                    };
                    return EvaluatedValue{
                        .Number = result,
                    };
                }
                return error.TypeMismatch;
            },
            .FunctionDefinition => {
                // 関数定義の評価はここでは行わない
                return error.InvalidNode;
            },
            .VariableDeclaration => {
               if (node.initializer) |init_node| {
                    const init_val = try self.evalute(init_node);
                    
                    if (node.variable_name) |var_name| {
                        if (self.symbol_table.find_symbol(var_name)) |symbol| {
                            if (init_val == .Number) {
                                _ = symbol;
                            }
                         }
                    }
                    return init_val;
               }
                return error.InvalidNode;
            },
            .StatementList => {
                var result: EvaluatedValue = undefined;
                for (node.statements.?.items) |stmt| {
                    const eval_result = try self.evalute(stmt);
                    defer eval_result.deinit(self.allocator);

                    result.deinit(self.allocator);

                    if (eval_result == .Number) {
                        result = EvaluatedValue{
                            .Number = eval_result.Number,
                        };
                    } else if (eval_result == .Identifier) {
                        const copied_id = try self.allocator.dupe(u8, eval_result.Identifier);
                        result = EvaluatedValue{
                            .Identifier = copied_id,
                        };
                    }
                }
                return result;
            },
            .SingleContainer => {
                // 単一コンテナの評価はここでは行わない
                return error.InvalidNode;
            },
            .ContainerChain => {
                // コンテナチェーンの評価はここでは行わない
                return error.InvalidNode;
            },

        }
    }

};

pub fn free_token_list(allocator: std.mem.Allocator, head: *Token) void {
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

    pub fn deinit(self: EvaluatedValue, allocator: std.mem.Allocator) void {
        switch (self) {
            .Identifier => |id_slice| {
                allocator.free(id_slice);
            },
            else => {},
        }
    }
};