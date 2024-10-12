const std = @import("std");
const print = std.debug.print;
const panic = std.debug.panic;

pub const Error = error{
    FileReadError,
    EmuExecError
};

pub const OpMode = enum {
    Register,
    Immediate,
    Displacement,
    Direct,
    RegisterIndirect,
    BasedIndexed,
    Indexed,
    Based,
    BasedIndexedDisplacement,
    String,
    Input,
    Output,
    Relative,
};

pub const DataSize = enum {
    Byte,
    Word,
};

pub const Immediate = struct {
    value: u16,
    size: DataSize,
};

pub const Register = enum {
    AH,
    BH,
    CH,
    DH,
    AL,
    BL,
    CL,
    DL,
    AX,
    BX,
    CX,
    DX,
    SI,
    DI,
    SP,
    BP,
    CS,
    DS,
    ES,
    SS,
};

pub const Operand = union(enum) {
    register: Register,
    immediate: Immediate,
};

pub const Cpu = struct {
    regs: [11]u16 = undefined,
    ip: u16,
    flags: u16,

    pub fn init(ip: u16) !Cpu {
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
        switch(regs) {
            .AX, .BX, .CX, .DX, .SI, .DI, .SP, .BP, .CS, .DS, .ES, .SS => |index| {
                return self.regs[@intFromEnum(index) - 8];
            },
            .AL, .BL, .CL, .DL => |index| {
                return self.regs[@intFromEnum(index) - 4] & 0xff;
            },
            .AH, .BH, .CH, .DH => |index| {
                return self.regs[@intFromEnum(index)] >> 8;
            },
        }
        
        return 0;
    }

    pub fn write_reg(self: *Cpu, regs: Register, value: u16) void {
        switch (regs) {
            .AX, .BX, .CX, .DX, .SI, .DI, .SP, .BP, .CS, .DS, .ES, .SS => |index| {
                self.regs[@intFromEnum(index) - 8] = value;
            },
            .AL, .BL, .CL, .DL => |index| {
                const r: u16 = self.regs[@intFromEnum(index) - 4];
                self.regs[@intFromEnum(index) - 4] = (r & 0xff00) | (value & 0xff);
            },
            .AH, .BH, .CH, .DH => |index| {
                const r: u16 = self.regs[@intFromEnum(index)];
                self.regs[@intFromEnum(index)] = ((value & 0xff) << 8) | (r & 0xff);
            },
        }
    }

    pub fn mov_to_register(self: *Cpu, reg: Register, oper: Operand) void {
        switch (oper) {
            .immediate => |imm| {
                switch (reg) {
                    .CS, .DS, .ES, .SS => panic("cannot copy immediate value to segment register", .{}),
                    else => self.write_reg(reg, imm.value)
                }                
            },
            .register => |register| {
                const val = self.read_reg(register);
                switch (reg) {
                    .CS, .DS, .ES, .SS => {
                        if (register == .CS or register == .DS or register == .ES or register == .SS) {
                            panic("cannot copy value of one segment register to another segment register", .{});
                        } else {
                            self.write_reg(reg, val);
                        }
                    },
                    else => self.write_reg(reg, val)
                }
            }
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

    pub fn init() !Emulator {
        return .{ 
            .cpu = try Cpu.init(0), 
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

    pub fn load_ram(self: *Emulator, addr: u16, data: []u8) void {
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

    pub fn exec(self: *Emulator) !void {
        try self.cpu.exec_instrution();
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var emu = try Emulator.init();
    try emu.load_rom(allocator, "example/test.bin");
    emu.load_ram(0x0, emu.rom);
    defer allocator.free(emu.rom);

    // print the contents of the rom
    for (0..emu.rom.len) |i| {
        print("{x} ", .{emu.rom[i]});
    }
    print("\n", .{});

    try emu.exec();
}
