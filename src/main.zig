const std = @import("std");

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
    HIGH = 0x0,
    LOW = 0x1,
    FULL = 0x2,
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
                .AX => @intCast((self.ax & 0xFF00) >> 8),
                .BX => @intCast((self.bx & 0xFF00) >> 8),
                .CX => @intCast((self.cx & 0xFF00) >> 8),
                .DX => @intCast((self.dx & 0xFF00) >> 8),
                else => std.debug.panic("Trying to read an invalid HIGH half register", .{})
            },
            .LOW => switch (regs) {
                .AX => @intCast(self.ax & 0x00FF),
                .BX => @intCast(self.bx & 0x00FF),
                .CX => @intCast(self.cx & 0x00FF),
                .DX => @intCast(self.dx & 0x00FF),
                else => std.debug.panic("Trying to read an invalid LOW half register", .{})
            },
            else => std.debug.panic("Trying to read a 16bit register in 8bit reading mode", .{})
        };
    }

    pub fn write_reg(self: *Cpu, regs: Register, half: RegisterHalf, val: u16) void {
        switch (half) {
            .FULL => switch (regs) {
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
            },
            .HIGH => switch (regs) {
                .AX => self.ax = val & 0xFF00,
                .BX => self.bx = val & 0xFF00,
                .CX => self.cx = val & 0xFF00,
                .DX => self.dx = val & 0xFF00,
                else => std.debug.panic("Trying to read to an invalid HIGH half register", .{})
            },
            .LOW => switch (regs) {
                .AX => self.ax = val & 0x00FF,
                .BX => self.bx = val & 0x00FF,
                .CX => self.cx = val & 0x00FF,
                .DX => self.dx = val & 0x00FF,
                else => std.debug.panic("Trying to read to an invalid LOW half register", .{})
            },
            _ => {}
        }
    }
};

pub const Emulator = struct {
    cpu: Cpu,
    ram: [0x100000]u8,
    rom: []u8, 

    pub fn init() !Emulator { 
        var emu = std.mem.zeroes(Emulator);
        emu.cpu = try Cpu.init(0);
        emu.ram = [_]u8{0}**0x100000;
        return emu;
    }

    pub fn loadRom(self: *Emulator, allocator: std.mem.Allocator, path: []const u8) ![]u8 {
        _ = self;
        _ = allocator;
    }
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer gpa.deinit();

    var emu = try Emulator.init();
    emu.loadRom(allocator, "example/test.bin");

    var cpu = emu.cpu;


    cpu.write_reg(.AX, .FULL, 0xd0f0);
    std.debug.print("{x}", .{cpu.read_reg8(.AX, .HIGH)});
}
