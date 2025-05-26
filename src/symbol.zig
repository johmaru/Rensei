const std = @import("std");

pub const SymbolKind = enum {
    Variable,
    Function,
    Type
};

// 後でより複雑な型システムを実装する
pub const SymbolType = enum {
    Int,
    Boolean,
    String,
};


pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind,
    symbol_type: SymbolType,
    address: usize,
    value: ?u8,
    scope: *Scope,
    attributes: ?*std.ArrayList([]const u8),

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, kind: SymbolKind, symbol_type: SymbolType, scope: *Scope) !*Symbol {
        const self = try allocator.create(Symbol);
        errdefer allocator.destroy(self);

        const name_copy = try allocator.dupe(u8, name);
        errdefer allocator.free(name_copy);

        self.* = .{
            .name = name_copy,
            .kind = kind,
            .symbol_type = symbol_type,
            .address = 0,
            .value = null,
            .scope = scope,
            .attributes = null,
            .allocator = allocator,

        };
        return self;
    }

    pub fn deinit(self: *Symbol) void {
        self.allocator.free(self.name);
        if (self.attributes) |attrs| {
            for (attrs.items) |attr| {
                self.allocator.free(attr);
            }
            attrs.deinit();
            self.allocator.destroy(attrs);
        }
        self.allocator.destroy(self);
    }

    pub fn addAttribute(self: *Symbol, attribute_name: []const u8) !void {
        if (self.attributes == null) {
            const new_list = try self.allocator.create(std.ArrayList([]const u8));
            errdefer self.allocator.destroy(new_list);

            new_list.* = std.ArrayList([]const u8).init(self.allocator);
            self.attributes = new_list;
        }

        const attr_copy = try self.allocator.dupe(u8, attribute_name);
        errdefer self.allocator.free(attr_copy);

        try self.attributes.?.append(attr_copy);
    }
};

pub const Scope = struct {
    parent: ?*Scope,
    symbols: std.ArrayList(*Symbol),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*Scope {
        const self = try allocator.create(Scope);
        errdefer allocator.destroy(self);

        self.* = .{
            .parent = null,
            .symbols = std.ArrayList(*Symbol).init(allocator),
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Scope) void {
        for (self.symbols.items) |symbol| {
            symbol.deinit();
        }
        self.symbols.deinit();
        self.allocator.destroy(self);
    }

    pub fn add_symbol(self: *Scope, symbol: *Symbol) !void {
        try self.symbols.append(symbol);
    }

    pub fn find_symbol(self: *Scope, name: []const u8) ?*Symbol {
        for (self.symbols.items) |symbol| {
            if (std.mem.eql(u8, symbol.name, name)) {
                return symbol;
            }
        }
        if (self.parent) |parent_scope| {
            return parent_scope.find_symbol(name);
        }
        return null;
    }
    pub fn create_child_scope(self: *Scope) !*Scope {
        const child_scope = try Scope.init(self.allocator);
        child_scope.parent = self;
        return child_scope;
    }

    pub fn destroy(self: *Scope) void {
        self.deinit();
    }
    
    pub fn print(self: *Scope) void {
        for (self.symbols.items) |symbol| {
            std.debug.print("Symbol: {s}, Kind: {any}, Type: {any}\n", .{symbol.name, symbol.kind, symbol.symbol_type});
        }
    }
};

pub const SymbolTable = struct {
    global_scope: *Scope,
    current_scope: *Scope,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) !*SymbolTable {
        const self = try allocator.create(SymbolTable);
        errdefer allocator.destroy(self);

        const global = try Scope.init(allocator);

        self.* = .{
            .global_scope = global,
            .current_scope = global,
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *SymbolTable) void {
        while (self.current_scope != self.global_scope) {
            const  parent = self.current_scope.parent orelse {
                std.debug.panic("Current scope has no parent, cannot exit scope.", .{});
                break;
            };
            const old_scope = self.current_scope;
            self.current_scope = parent;
            old_scope.deinit();
        }

        self.global_scope.deinit();
        self.allocator.destroy(self);
    }

    pub fn add_symbol(self: *SymbolTable, name: []const u8, kind: SymbolKind, symbol_type: SymbolType) !*Symbol {
        const symbol = try Symbol.init(self.allocator, name, kind, symbol_type, self.current_scope);

        errdefer symbol.deinit();
        try self.current_scope.add_symbol(symbol);
        return symbol;
    }

    pub fn find_symbol(self: *SymbolTable, name: []const u8) ?*Symbol {
        return self.current_scope.find_symbol(name);
    }
    
    pub fn enter_scope(self: *SymbolTable) !void {
        const new_scope = try self.current_scope.create_child_scope();
        self.current_scope = new_scope;
    }

    pub fn exit_scope(self: *SymbolTable) void {
        if (self.current_scope.parent) |parent_scope| {
           const old_scope = self.current_scope;
            self.current_scope = parent_scope;
            old_scope.deinit();
        }
    }

    pub fn print(self: *SymbolTable) void {
        std.debug.print("Global Scope:\n", .{});
        self.global_scope.print();
        if (self.current_scope != self.global_scope) {
            std.debug.print("Current Scope (different from global):\n", .{});
            self.current_scope.print();
        } else {
            std.debug.print("Current Scope (same as global):\n", .{});
        }
    }

    pub fn destroy(self: *SymbolTable) void {
        self.deinit();
    }

    pub fn get_current_scope(self: *SymbolTable) *Scope {
        return self.current_scope;
    }

    pub fn get_global_scope(self: *SymbolTable) *Scope {
        return self.global_scope;
    }

    pub fn get_allocator(self: *SymbolTable) std.mem.Allocator {
        return self.allocator;
    }
};

