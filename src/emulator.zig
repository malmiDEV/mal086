const std = @import("std");
const print = std.debug.print;
const panic = std.debug.panic;

pub const Error = error{
    FileReadError,
    WriteSegRegError,
    InvalidInstructionError,
};

pub const Register = enum {
    AL,
    CL,
    DL,
    BL,
    AH,
    CH,
    DH,
    BH,
    AX,
    CX,
    DX,
    BX,
    SP,
    BP,
    SI,
    DI,
    ES,
    SS,
    CS,
    DS,
};

pub const DataSize = enum {
    Byte,
    Word,
};

pub const ModRm = enum {
    Reg,
    Seg,
};

pub const Immediate = struct {
    data: union(enum) {
        modrm: ModRm,
        reg: Register,
        int: u16,
    },
    value: u16,
    size: DataSize,
};

pub const Prefix = enum(u8) {
    CS = 0x2e,
    DS = 0x3e,
    ES = 0x26,
    SS = 0x36,
    _,
};

pub const Displacement = struct {
    lo: i8,
    hi: i16,
};

pub const Operand = union(enum) {
    register: Register,
    immediate: Immediate,
    displacement: Displacement,
};

pub const Opcode = enum {
    MOV,
    ADD,
    SUB,
    CMP,
    JE,
    JL,
    JLE,
    JB,
    JBE,
    JP,
    JO,
    JS,
    JNZ,
    JNL,
    JNLE,
    JNB,
    JNBE,
    JNP,
    JNO,
    JNS,
    LOOP,
    LOOPZ,
    LOOPNZ,
    JCXZ,
};

pub const Instruction = struct {
    prefix: ?Prefix = null,
    opcode: Opcode,
    operand: Operand,
    size: u8,
};

fn generate_instruction_map(allocator: std.mem.Allocator) !std.AutoHashMap(u8, Instruction) {
    var map = std.AutoHashMap(u8, Instruction).init(allocator);

    // mov to register immediate
    //
    // mov al, ib .. mov bh, ib
    //
    try map.put(0xb0, Instruction{
        .prefix = null,
        .opcode = .MOV,
        .operand = Operand{ 
            .immediate = Immediate{ 
                .data = .{.reg = .AL }, 
                .value = 0, 
                .size = .Byte 
            } 
        },
        .size = 0,
    });
    try map.put(0xb1, Instruction{
        .prefix = null,
        .opcode = .MOV,
        .operand = Operand{ 
            .immediate = Immediate{ 
                .data = .{.reg = .CL }, 
                .value = 0, 
                .size = .Byte 
            } 
        },
        .size = 0,
    });
    try map.put(0xb2, Instruction{
        .prefix = null,
        .opcode = .MOV,
        .operand = Operand{ 
            .immediate = Immediate{ 
                .data = .{.reg = .DL }, 
                .value = 0, 
                .size = .Byte 
            } 
        },
        .size = 0,
    });
    try map.put(0xb3, Instruction{
        .prefix = null,
        .opcode = .MOV,
        .operand = Operand{ 
            .immediate = Immediate{ 
                .data = .{.reg = .BL }, 
                .value = 0, 
                .size = .Byte 
            } 
        },
        .size = 0,
    });
    try map.put(0xb4, Instruction{
        .prefix = null,
        .opcode = .MOV,
        .operand = Operand{ 
            .immediate = Immediate{ 
                .data = .{.reg = .AH }, 
                .value = 0, 
                .size = .Byte 
            } 
        },
        .size = 0,
    });
    try map.put(0xb5, Instruction{
        .prefix = null,
        .opcode = .MOV,
        .operand = Operand{ 
            .immediate = Immediate{ 
                .data = .{.reg = .CH }, 
                .value = 0, 
                .size = .Byte 
            } 
        },
        .size = 0,
    });
    try map.put(0xb6, Instruction{
        .prefix = null,
        .opcode = .MOV,
        .operand = Operand{ 
            .immediate = Immediate{ 
                .data = .{.reg = .DH }, 
                .value = 0, 
                .size = .Byte 
            } 
        },
        .size = 0,
    });
    try map.put(0xb7, Instruction{
        .prefix = null,
        .opcode = .MOV,
        .operand = Operand{ 
            .immediate = Immediate{ 
                .data = .{.reg = .BH }, 
                .value = 0, 
                .size = .Byte 
            } 
        },
        .size = 0,
    });

    //
    // mov ax, iv .. mov di, iv
    //

    return map;
}

pub const Cpu = struct {
    regs: [11]u16 = undefined,
    ip: u16,
    flags: u16,

    pub fn init(ip: u16) Cpu {
        return .{
            .regs = [_]u16{0} ** 11,
            .ip = ip,
            .flags = 0,
        };
    }

    pub fn reset(self: *Cpu, ip: u16) void {
        // reset regs
        self.write_reg(.AX, 0);
        self.write_reg(.BX, 0);
        self.write_reg(.CX, 0);
        self.write_reg(.DX, 0);
        self.write_reg(.SI, 0);
        self.write_reg(.DI, 0);
        self.write_reg(.SP, 0);
        self.write_reg(.BP, 0);
        self.write_reg(.CS, 0);
        self.write_reg(.DS, 0);
        self.write_reg(.ES, 0);
        self.write_reg(.SS, 0);
        self.ip = ip;
        self.flags = 0;
    }

    pub fn read_reg(self: *Cpu, regs: Register) u16 {
        switch (regs) {
            .AX, .CX, .DX, .BX, .SP, .BP, .SI, .DI, .ES, .SS, .CS, .DS  => |index| {
                return self.regs[@intFromEnum(index) - 8];
            },
            .AL, .CL, .DL, .BL=> |index| {
                return self.regs[@intFromEnum(index)] & 0xff;
            },
            .AH, .CH, .DH, .BL=> |index| {
                return self.regs[@intFromEnum(index) - 4] >> 8;
            },
        }

        return 0;
    }

    pub fn write_reg(self: *Cpu, regs: Register, value: u16) void {
        switch (regs) {
            .AX, .CX, .DX, .BX, .SP, .BP, .SI, .DI, .ES, .SS, .CS, .DS => |index| {
                self.regs[@intFromEnum(index) - 8] = value;
            },
            .AL, .CL, .DL  .BL,=> |index| {
                const r: u16 = self.regs[@intFromEnum(index)];
                self.regs[@intFromEnum(index)] = (r & 0xff00) | (value & 0xff);
            },
            .AH, .CH, .DH  .BH,=> |index| {
                const r: u16 = self.regs[@intFromEnum(index)];
                self.regs[@intFromEnum(index) - 4] = ((value & 0xff) << 8) | (r & 0xff);
            },
        }
    }

    pub fn mov_to_register(self: *Cpu, reg: Register, oper: Operand) !void {
        switch (oper) {
            .immediate => |imm| {
                switch (reg) {
                    .CS, .DS, .ES, .SS => return Error.WriteSegRegError,
                    else => self.write_reg(reg, imm.value),
                }
            },
            .register => |register| {
                const val = self.read_reg(register);
                switch (reg) {
                    .CS, .DS, .ES, .SS => {
                        if (register == .CS or register == .DS or register == .ES or register == .SS) {
                            return Error.WriteSegRegError;
                        } else {
                            self.write_reg(reg, val);
                        }
                    },
                    else => self.write_reg(reg, val),
                }
            },
        }
    }

    pub fn exec_instrution(self: *Cpu) !void {
        _ = self;
    }
};

pub const Emulator = struct {
    cpu: Cpu,
    ram: [0x100000]u8,
    rom: []u8,

    pub fn init() Emulator {
        return .{
            .cpu = Cpu.init(0x7c00),
            .ram = [_]u8{0} ** 0x100000,
            .rom = undefined,
        };
    }

    pub fn write_mem8(self: *Emulator, addr: u16, value: u8) void {
        self.ram[@intCast(addr)] = value;
    }

    pub fn read_mem8(self: *Emulator, addr: u16) u8 {
        return self.ram[@intCast(addr)];
    }

    // x86 uses little endian
    pub fn write_mem16(self: *Emulator, addr: u16, value: u16) void {
        self.ram[@intCast(addr)] = @intCast(value & 0xff);
        self.ram[@intCast(addr + 1)] = @intCast((value & 0xff00) >> 8);
    }

    pub fn read_mem16(self: *Emulator, addr: u16) u16 {
        const lo: u16 = @intCast(self.ram[@intCast(addr)]);
        const hi: u16 = @intCast(self.ram[@intCast(addr + 1)]);
        return lo & 0xff | hi << 8;
    }

    pub fn load_ram(self: *Emulator, addr: u16, data: []const u8) void {
        for (0..data.len) |i| {
            self.write_mem8(@intCast(addr + i), data[i]);
        }
    }

    pub fn load_rom(self: *Emulator, allocator: std.mem.Allocator, path: []const u8) !void {
        const abs_path = try std.fs.realpathAlloc(allocator, path);
        defer allocator.free(abs_path);

        const file = try std.fs.openFileAbsolute(abs_path, .{ .mode = .read_only });
        defer file.close();

        const file_info = try file.stat();
        const file_size = file_info.size;

        self.rom = try allocator.alloc(u8, file_size);
        const byte_read = try file.read(self.rom);
        if (byte_read != file_size) {
            return Error.FileReadError;
        }
    }

    // knowledge test
    pub fn debug_inst(self: *Emulator, allocator: std.mem.Allocator) !Instruction {
        var cpu = self.cpu;
        var pseudo_ip: u16 = 0;
        var size: u8 = 0;
        _ = size;

        // check segment override prefix
        var prefix: ?Prefix = blk: {
            switch (@as(Prefix, @enumFromInt(self.read_mem8(pseudo_ip+cpu.ip)))) {
                .CS, .DS, .ES, .SS => |data| {
                    pseudo_ip += 1;
                    break :blk data;
                },
                else => break :blk null,
            }
        };

        var map = try generate_instruction_map(allocator);
        defer map.deinit();
        
        var opcode: Opcode = undefined;
        var operand: Operand = undefined;
        switch (self.read_mem8(pseudo_ip+cpu.ip)) {
            // mov ah, ib .. mov bh, ib
            0xb0, 0xb1, 0xb2, 0xb3, 0xb4, 0xb5, 0xb6, 0xb7 => |b| {
                var inst = map.get(b).?;
                const inst_op = inst.operand.immediate;
                switch (inst_op.size) {
                    .Byte => pseudo_ip += 1,
                    .Word => pseudo_ip += 2,
                }
                const value = self.read_mem8(pseudo_ip+cpu.ip);
                opcode = inst.opcode;
                operand = Operand{.immediate=Immediate{.data=inst_op.data, .value = value, .size=inst_op.size}};
            },
            else => return Error.InvalidInstructionError,
        }

        return .{ 
            .prefix = prefix, 
            .opcode = opcode, 
            .operand = operand, 
            .size = @intCast(pseudo_ip) 
        };
    }

    pub fn exec(self: *Emulator) !void {
        try self.cpu.exec_instrution();
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var emu = Emulator.init();
    try emu.load_rom(allocator, "example/test.bin");
    emu.load_ram(0x7c00, emu.rom);
    defer allocator.free(emu.rom);

    //mov ax, 0xdead
    //mov bx, ax
    // print the contents of the rom
    for (0..emu.rom.len) |i| {
        print("{x} ", .{emu.rom[i]});
    }
    print("\n", .{});

    const inst = try emu.debug_inst(allocator);
    print("prefix: {?}\n", .{inst.prefix});
    print("opcode: {?}\n", .{inst.opcode});
    print("operand: {?}\n", .{inst.operand.immediate.data.reg});
    print("imm val: 0x{x}\n", .{inst.operand.immediate.value});
    print("size: {?}\n", .{inst.size});

    try emu.exec();
}
