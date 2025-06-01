const std = @import("std");

pub const SymbolKind = enum {
    Variable,
    Function,
    Type,
    Container,
};

// 後でより複雑な型システムを実装する
pub const SymbolType = enum {
    Int,
    Boolean,
    String,
};

pub const value_type = union(enum) {
    Int: i64,
    Boolean: bool,
    String: []const u8,

    pub fn deinit(self: value_type, allocator: std.mem.Allocator) void {
        switch (self) {
            .String => |str| {
                allocator.free(str);
            },
            else => {},
        }
    }
};


pub const Symbol = struct {
    name: []const u8,
    kind: SymbolKind,
    symbol_type: SymbolType,
    address: usize,
    value: ?value_type,
    scope: *Scope,
    attributes: ?*std.ArrayList([]const u8),

    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, name: []const u8, kind: SymbolKind, symbol_type: SymbolType, scope: *Scope,initial_value: ?value_type) !*Symbol {
        const self = try allocator.create(Symbol);
        errdefer allocator.destroy(self);

        const name_copy = try allocator.dupe(u8, name);
        errdefer allocator.free(name_copy);

        self.* = .{
            .name = name_copy,
            .kind = kind,
            .symbol_type = symbol_type,
            .address = 0,
            .value = initial_value,
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
        if (self.value) |val| {
            val.deinit(self.allocator);
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

    pub fn remove(symbol: *Symbol) !void {
        symbol.deinit();
    }

    pub fn get_scope(self: *Symbol) *Scope {
        return self.scope;
    }
};

pub const ScopeKind = enum {
    Global,
    Local,
    Function,
    Container,
    Block,
};

pub const Scope = struct {
    name: ?[]const u8,
    scope_kind: ScopeKind,
    parent: ?*Scope,
    symbols: std.ArrayList(*Symbol),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator,kind: ScopeKind, parent: ?*Scope) !*Scope {
        const self = try allocator.create(Scope);
        errdefer allocator.destroy(self);

        self.* = .{
            .name = null,
            .scope_kind = kind,
            .parent = parent,
            .symbols = std.ArrayList(*Symbol).init(allocator),
            .allocator = allocator,
        };
        return self;
    }

    pub fn deinit(self: *Scope) void {
       if (self.name) |name| {
            self.allocator.free(name);
        }
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
    pub fn create_child_scope(self: *Scope,kind: ScopeKind) !*Scope {
        const child_scope = try Scope.init(self.allocator,kind, self);
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

        const global = try Scope.init(allocator, ScopeKind.Global, null);

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

    pub fn add_symbol(self: *SymbolTable, name: []const u8, kind: SymbolKind, symbol_type: SymbolType, initial_value: ?value_type) !*Symbol {
        const symbol = try Symbol.init(self.allocator, name, kind, symbol_type, self.current_scope, initial_value);

        errdefer symbol.deinit();
        try self.current_scope.add_symbol(symbol);
        return symbol;
    }

    pub fn delete_symbol(self: *SymbolTable, name: []const u8) !void {
        const symbol = self.current_scope.find_symbol(name) orelse {
            return error.SymbolNotFound;
        };

        var found_index: ?usize = null;
        for (self.current_scope.symbols.items, 0..) |s,i| {
            if (std.mem.eql(u8, s.name, symbol)) {
                found_index = i;
                break;
            }
        }

        if (found_index) |index| {
            const removed_symbol = self.current_scope.symbols.swapRemove(index);
            removed_symbol.deinit();
        } else {
            return error.SymbolNotFound;
        }
    }

    pub fn find_symbol(self: *SymbolTable, name: []const u8) ?*Symbol {
        return self.current_scope.find_symbol(name);
    }
    
    pub fn enter_scope(self: *SymbolTable,kind: ScopeKind) !void {
        const new_scope = try self.current_scope.create_child_scope(kind);
        self.current_scope = new_scope;
    }

    pub fn enter_scope_with_name(self: *SymbolTable, name: []const u8,kind: ScopeKind) !void {
        const new_scope = try self.current_scope.create_child_scope(kind);
        new_scope.name = try self.allocator.dupe(u8, name);
        self.current_scope = new_scope;
    }

    pub fn exit_scope(self: *SymbolTable) void {
        if (self.current_scope.parent) |parent_scope| {
           const old_scope = self.current_scope;
            self.current_scope = parent_scope;
            old_scope.deinit();
        }
    }

    pub fn exit_scope_with_name(self: *SymbolTable, name: []const u8) !void {
        if (self.current_scope.parent) |parent_scope| {
            if (self.current_scope.name) |current_scope_name| {
                if (std.mem.eql(u8, current_scope_name, name)) {
                const old_scope = self.current_scope;
                self.current_scope = parent_scope;
                old_scope.deinit();
            } else {
                return error.ScopeNameMismatch;
            }
            }
        } else {
            return error.NoParentScope;
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

