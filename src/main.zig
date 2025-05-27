pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator= gpa.allocator();

    const source = "int x = 10 + 5; int y = x * 2; int z = x + y;";

    std.debug.print("Source: \"{s}\"\n", .{source});

  
    var tokenizer = try Tokenizer.init(allocator, source);

    defer tokenizer.deinit();


    const first_token = tokenizer.tokenize() catch |err| {
        std.debug.print("Tokenization failed: {any}\n", .{err});
        return;
    };

    if (first_token == null) {
        std.debug.print("No tokens produced.\n", .{});
        return;
    }
    defer free_token_list(allocator, first_token.?); 

   
    std.debug.print("Tokens:\n", .{});
    var dbg_tok = first_token;
    while (dbg_tok) |t| {
        std.debug.print("  - Kind: {any}, Idx: {d}, Len: {d}", .{t.kind, t.string_index, t.length});
        if (t.kind == .Number) {
            std.debug.print(" (\"{s}\")", .{source[t.string_index .. t.string_index + t.length]});
        }
        std.debug.print("\n", .{});
        if (t.kind == .EndOfFile) break;
        dbg_tok = t.next;
    }

    const symbol_table = try Symbol_Table.init(allocator);
    defer symbol_table.deinit();
    
    var parser = Parser.init(allocator, first_token, source, symbol_table);

    while (parser.current_token != null and parser.current_token.?.kind != .EndOfFile) {
        const ast_node = parser.parse_statement() catch |err| {
            std.debug.print("Parsing failed: {any}\n", .{err});
            // [修正] エラー発生時に残りのトークンも表示してみる（デバッグ用）
            var err_tok = parser.current_token;
            std.debug.print("Remaining tokens at error:\n", .{});
            while (err_tok) |et| {
                std.debug.print("  - Kind: {any}, Idx: {d}, Len: {d}\n", .{et.kind, et.string_index, et.length});
                if (et.kind == .EndOfFile) break;
                err_tok = et.next;
            }
            return;
        };
        defer ast_node.deinit();

         std.debug.print("AST Node Kind: {any}", .{ast_node.kind});
        if (ast_node.kind == .NumberLiteral) {
            std.debug.print(", Value: {?}\n", .{ast_node.value});
        } else if (ast_node.kind == .Identifier) {
            std.debug.print(", Value: {s}\n", .{ast_node.identifier_value.?});
        } else if (ast_node.kind == .VariableDeclaration) {
            std.debug.print(", Name: {s}, Type: {s}", .{ast_node.variable_name.?, ast_node.variable_type.?});
            if (ast_node.initializer) |init_node| {
                std.debug.print(", Initializer: present (kind: {any})\n", .{init_node.kind});
            } else {
                std.debug.print(", Initializer: none\n", .{});
            }
        } else if (ast_node.kind == .BinaryOperation) {
             std.debug.print(", Operator: {any}\n", .{ast_node.operator.?});
        } else if (ast_node.kind == .UnaryOperation) {
            std.debug.print(", Operator: {any}\n", .{ast_node.unary_operator.?});
        }
         else {
            std.debug.print("\n", .{});
        }

        const result = parser.evalute(ast_node) catch |err| {
            if (err == error.InvalidNode) { 
                 std.debug.print("Evaluation not applicable for this node type or not yet implemented ({any}).\n", .{ast_node.kind});
            } else {
                std.debug.print("Evaluation failed: {any}\n", .{err});
            }
            std.debug.print("---\n", .{});
            continue; 
        };
        defer result.deinit(allocator);
        // test処理
        switch (result) {
            .Number => |num_val| {
                std.debug.print("Result (Number): {d}\n", .{num_val});
            },
            .Identifier => |id_str| {
                std.debug.print("Result (Identifier): {s}\n", .{id_str});
            },
        }
        std.debug.print("---\n", .{});
    }

    std.debug.print("Final Symbol Table:\n", .{});
    symbol_table.print();

    const symbol_z = symbol_table.find_symbol("z") orelse {
        std.debug.print("Symbol 'z' not found in the symbol table.\n", .{});
        return;
    };
    if (symbol_z.value) |value| {
        std.debug.print("Symbol 'z' found with value: {d}\n", .{value.Int});
    } else {
        std.debug.print("Symbol 'z' found but no value assigned.\n", .{});
    }
}

const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const parser_module = @import("parser.zig");
const Symbol_Table = @import("symbol.zig").SymbolTable;
const Parser = parser_module.Parser;
const AstNode = parser_module.AstNode;
const free_token_list = parser_module.free_token_list;
const EvaluatedValue = parser_module.EvaluatedValue;