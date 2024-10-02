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

pub const Register = enum(u4) {
    AX = 0x0,
    BX = 0x1,
    CX = 0x2,
    DX = 0x3,
    SI = 0x4,
    DI = 0x5,
    SP = 0x6,
    BP = 0x7,
    CS = 0x8,
    DS = 0x9,
    ES = 0xa,
    SS = 0xb,
    _   
};

pub const RegisterHalf = enum(u3) {
    LOW = 0x1,
    HIGH = 0x2,
    _
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
        var sys = std.mem.zeroes(Cpu);
        sys.ip = ip;
        return sys;
    }

    pub fn read_reg16(self: *Cpu, regs: Register) u16 {
        return switch (regs) {
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
            else => std.debug.panic("Trying to read an invalid register", .{})
        };
    }

    pub fn read_reg8(self: *Cpu, regs: Register, half: RegisterHalf) u8 {
        return switch (half) {
            .HIGH => switch (regs) {
                .AX => @intCast((self.ax & 0xff00) >> 8),
                .BX => @intCast((self.bx & 0xff00) >> 8),
                .CX => @intCast((self.cx & 0xff00) >> 8),
                .DX => @intCast((self.dx & 0xff00) >> 8),
                else => std.debug.panic("Trying to read an invalid HIGH half register", .{})
            },
            .LOW => switch (regs) {
                .AX => @intCast(self.ax & 0xff),
                .BX => @intCast(self.bx & 0xff),
                .CX => @intCast(self.cx & 0xff),
                .DX => @intCast(self.dx & 0xff),
                else => std.debug.panic("Trying to read an invalid LOW half register", .{})
            },
            else => std.debug.panic("Trying to read a 16bit register in 8bit reading mode", .{})
        };
    }

    pub fn write_reg16(self: *Cpu, regs: Register, val: u16) void {
        switch (regs) {
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
            else => std.debug.panic("Trying to read to an invalid register", .{})
        }
    }

    pub fn write_reg8(self: *Cpu, regs: Register, half: RegisterHalf, val: u8) void {
        switch (half) {
            .HIGH => switch (regs) {
                .AX => self.ax = (self.ax & 0xff) | @as(u16, @intCast(val)) << 8,
                .BX => self.bx = (self.bx & 0xff) | @as(u16, @intCast(val)) << 8,
                .CX => self.cx = (self.cx & 0xff) | @as(u16, @intCast(val)) << 8,
                .DX => self.dx = (self.dx & 0xff) | @as(u16, @intCast(val)) << 8,
                else => std.debug.panic("Trying to access an invalid HIGH half register", .{})
            },
            .LOW => switch (regs) {
                .AX => self.ax = (self.ax & 0xff00) | @as(u16, @intCast(val)),
                .BX => self.bx = (self.bx & 0xff00) | @as(u16, @intCast(val)),
                .CX => self.cx = (self.cx & 0xff00) | @as(u16, @intCast(val)),
                .DX => self.dx = (self.dx & 0xff00) | @as(u16, @intCast(val)),
                else => std.debug.panic("Trying to access an invalid LOW half register", .{})
            },
            else => std.debug.panic("Trying to access a 16bit register in 8bit writing mode", .{})
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
            .rom = undefined
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
        self.ram[@intCast(addr+1)] = @intCast((value & 0xff00) >> 8);
    }

    pub fn read_mem16(self: *Emulator, addr: u16) u16 {
        const lo: u16 = @intCast(self.ram[@intCast(addr)]);
        const hi: u16 = @intCast(self.ram[@intCast(addr+1)]);
        return lo & 0xff | hi << 8;
    }

    pub fn load_ram(self: *Emulator, addr: u16, data: []u8) void {
        for (0..data.len) |i| {
            self.write_mem8(@intCast(addr+i), data[i]);
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
        std.debug.print("{x} ",.{emu.rom[i]});
    }
    std.debug.print("\n",.{});

    // register test
    var cpu = emu.cpu;
    cpu.write_reg16(.AX, 0xc418);
    std.debug.print("reg ax: 0x{x}\n",.{cpu.read_reg16(.AX)});
    cpu.write_reg8(.AX, .LOW, 0xef);
    cpu.write_reg8(.AX, .HIGH, 0xbe);
    std.debug.print("reg ah: 0x{x}\n",.{cpu.read_reg8(.AX, .HIGH)});
    std.debug.print("reg al: 0x{x}\n",.{cpu.read_reg8(.AX, .LOW)});
    std.debug.print("reg ax: 0x{x}\n",.{cpu.read_reg16(.AX)});

    // mem test
    emu.write_mem8(0xfe, 0xb0);
    emu.write_mem8(0xff, 0x02);
    emu.write_mem16(0xdead, 0xbeef);
    std.debug.print("8mem at 0x00: 0x{x}\n",.{emu.read_mem8(0x00)});
    std.debug.print("16mem at 0xfe: 0x{x}\n",.{emu.read_mem16(0xfe)});
    std.debug.print("16mem at 0xdead: 0x{x}\n",.{emu.read_mem16(0xdead)});
}
