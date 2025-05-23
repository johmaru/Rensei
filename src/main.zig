pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    var allocator_val= gpa.allocator();
    const allocator = &allocator_val;

    const source = "++ 13";

    std.debug.print("Source: \"{s}\"\n", .{source});

  
    var tokenizer = try Tokenizer.init(allocator, source);

    defer allocator.destroy(tokenizer);


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


    
    var parser = Parser.init(allocator, first_token, source);

    while (parser.current_token != null and parser.current_token.?.kind != .EndOfFile) {
        const ast_node = parser.parse_expression() catch |err| {
            std.debug.print("Parsing failed: {any}\n", .{err});
            break;
        };
        defer ast_node.deinit();

        std.debug.print("AST Node Kind: {any}", .{ast_node.kind});
        if (ast_node.kind == .NumberLiteral) {
            std.debug.print(", Value: {?}\n", .{ast_node.value});
        } else if (ast_node.kind == .Identifier) {
            std.debug.print(", Value: {s}\n", .{ast_node.identifier_value.?});
        } else {
            std.debug.print("\n", .{});
        }

        const result = Parser.evalute(&parser, ast_node) catch |err| {
            std.debug.print("Evaluation failed: {any}\n", .{err});
            break;
        };
        defer result.deinit(allocator);
        // test処理
        switch (result) {
            .Number => |num_val| {
                if (num_val == 19) { 
                    std.debug.print("Result is 19!\n", .{});
                } else {
                    std.debug.print("Result (Number): {d}\n", .{num_val});
                }
            },
            .Identifier => |id_str| {
                std.debug.print("Result (Identifier): {s}\n", .{id_str});
            },
        }
        std.debug.print("---\n", .{});
    }
}

const std = @import("std");
const Tokenizer = @import("tokenizer.zig").Tokenizer;
const parser_module = @import("parser.zig");
const Parser = parser_module.Parser;
const AstNode = parser_module.AstNode;
const free_token_list = parser_module.free_token_list;
const EvaluatedValue = parser_module.EvaluatedValue;