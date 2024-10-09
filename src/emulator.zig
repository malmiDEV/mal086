const std = @import("std");

pub const Error = error{
    FileReadError,
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

pub const Operand = union(enum) {
    register: Register,
    immediate: Immediate,
};

pub const RegisterType = enum(u4) {
    AX = 0,
    BX = 1,
    CX = 2,
    DX = 3,
    SI = 4,
    DI = 5,
    SP = 6,
    BP = 7,
    CS = 8,
    DS = 9,
    ES = 10,
    SS = 11,
    _,
};

pub const RegisterHalf = enum(u3) {
    LOW = 1,
    HIGH = 2,
    FULL = 3,
    _,
};

pub const Register = struct {
    reg: RegisterType,
    half: RegisterHalf,
};

pub const Cpu = struct {
    ax: u16,
    bx: u16,
    cx: u16,
    dx: u16,
    si: u16,
    di: u16,
    sp: u16,
    bp: u16,
    cs: u16,
    ds: u16,
    es: u16,
    ss: u16,
    ip: u16,
    flags: u16,

    pub fn init(ip: u16) !Cpu {
        return .{
            .ax = 0,
            .bx = 0,
            .cx = 0,
            .dx = 0,
            .si = 0,
            .di = 0,
            .sp = 0,
            .bp = 0,
            .cs = 0,
            .ds = 0,
            .es = 0,
            .ss = 0,
            .ip = ip,
            .flags = 0,
        };
    }

    pub fn reset(self: *Cpu, ip: u16) void {
        // reset regs
        self.write_reg16(.AX, 0);
        self.write_reg16(.BX, 0);
        self.write_reg16(.CX, 0);
        self.write_reg16(.DX, 0);
        self.write_reg16(.SI, 0);
        self.write_reg16(.DI, 0);
        self.write_reg16(.SP, 0);
        self.write_reg16(.BP, 0);
        self.write_reg16(.CS, 0);
        self.write_reg16(.DS, 0);
        self.write_reg16(.ES, 0);
        self.write_reg16(.SS, 0);
        self.ip = ip;
        self.flags = 0;
    }

    pub fn read_reg16(self: *Cpu, regs: Register) u16 {
        return switch (regs.reg) {
            .AX => self.ax,
            .BX => self.bx,
            .CX => self.cx,
            .DX => self.dx,
            .SI => self.si,
            .DI => self.di,
            .SP => self.sp,
            .BP => self.bp,
            .CS => self.cs,
            .DS => self.ds,
            .ES => self.es,
            .SS => self.ss,
            else => std.debug.panic("Trying to read an invalid register", .{}),
        };
    }

    pub fn read_reg8(self: *Cpu, regs: Register) u8 {
        return switch (regs.half) {
            .HIGH => switch (regs.reg) {
                .AX => @intCast((self.ax & 0xff00) >> 8),
                .BX => @intCast((self.bx & 0xff00) >> 8),
                .CX => @intCast((self.cx & 0xff00) >> 8),
                .DX => @intCast((self.dx & 0xff00) >> 8),
                else => std.debug.panic("Trying to read an invalid HIGH half register", .{}),
            },
            .LOW => switch (regs.reg) {
                .AX => @intCast(self.ax & 0xff),
                .BX => @intCast(self.bx & 0xff),
                .CX => @intCast(self.cx & 0xff),
                .DX => @intCast(self.dx & 0xff),
                else => std.debug.panic("Trying to read an invalid LOW half register", .{}),
            },
            else => std.debug.panic("Trying to read a 16bit register in 8bit reading mode", .{}),
        };
    }

    pub fn write_reg16(self: *Cpu, regs: Register, val: u16) void {
        switch (regs.reg) {
            .AX => self.ax = val,
            .BX => self.bx = val,
            .CX => self.cx = val,
            .DX => self.dx = val,
            .SI => self.si = val,
            .DI => self.di = val,
            .SP => self.sp = val,
            .BP => self.bp = val,
            .CS => self.cs = val,
            .DS => self.ds = val,
            .ES => self.es = val,
            .SS => self.ss = val,
            else => std.debug.panic("Trying to read to an invalid register", .{}),
        }
    }

    pub fn write_reg8(self: *Cpu, regs: Register, val: u8) void {
        switch (regs.half) {
            .HIGH => switch (regs.reg) {
                .AX => self.ax = (self.ax & 0xff) | @as(u16, @intCast(val)) << 8,
                .BX => self.bx = (self.bx & 0xff) | @as(u16, @intCast(val)) << 8,
                .CX => self.cx = (self.cx & 0xff) | @as(u16, @intCast(val)) << 8,
                .DX => self.dx = (self.dx & 0xff) | @as(u16, @intCast(val)) << 8,
                else => std.debug.panic("Trying to access an invalid HIGH half register", .{}),
            },
            .LOW => switch (regs.reg) {
                .AX => self.ax = (self.ax & 0xff00) | @as(u16, @intCast(val)),
                .BX => self.bx = (self.bx & 0xff00) | @as(u16, @intCast(val)),
                .CX => self.cx = (self.cx & 0xff00) | @as(u16, @intCast(val)),
                .DX => self.dx = (self.dx & 0xff00) | @as(u16, @intCast(val)),
                else => std.debug.panic("Trying to access an invalid LOW half register", .{}),
            },
            else => std.debug.panic("Trying to access a 16bit register in 8bit writing mode", .{}),
        }
    }

    pub fn mov_to_register(self: *Cpu, reg: Register, oper: Operand) void {
        switch (oper) {
            .immediate => |imm| {
                switch (imm.size) {
                    .Word => {
                        self.write_reg16(reg, imm.value);
                    },
                    .Byte => {
                        self.write_reg8(reg, @intCast(imm.value));
                    },
                }
            },
            .register => |register| {
                switch (register.half) {
                    .FULL => {
                        const val: u16 = self.read_reg16(register);
                        self.write_reg16(reg, val);
                    },
                    .HIGH => {
                        const val: u8 = self.read_reg8(register);
                        self.write_reg8(reg, val);
                    },
                    .LOW => {
                        const val: u8 = self.read_reg8(register);
                        self.write_reg8(reg, val);
                    },
                    else => {}
                }
            }
        }
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
        std.debug.print("{x} ", .{emu.rom[i]});
    }
    std.debug.print("\n", .{});

    // register test
    var cpu = emu.cpu;
    cpu.mov_to_reg(Register{.reg=.AX,.half=.HIGH}, Operand{.immediate=Immediate{.value=0xbe,.size=.Byte}});
    cpu.mov_to_reg(Register{.reg=.AX,.half=.LOW}, Operand{.immediate=Immediate{.value=0xef,.size=.Byte}});
    std.debug.print("reg bx: 0x{x}\n", .{cpu.read_reg16(Register{.reg=.BX, .half=.FULL})});
    std.debug.print("reg ax: 0x{x}\n", .{cpu.read_reg16(Register{.reg=.AX, .half=.FULL})});
}
