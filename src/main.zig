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

pub const RegisterHalf = enum {
    HIGH,
    LOW,
    FULL,
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

    pub fn read_reg(self: *Cpu, regs: Register, half: RegisterHalf) u16 {
        return blk: { 
            switch (half) {
                RegisterHalf.FULL => switch (regs) {
                    Register.AX => break :blk self.ax,
                    Register.BX => break :blk self.bx,
                    Register.CX => break :blk self.cx,
                    Register.DX => break :blk self.dx,
                    Register.SI => break :blk self.si,
                    Register.DI => break :blk self.di,
                    Register.SP => break :blk self.sp,
                    Register.BP => break :blk self.bp,
                    Register.CS => break :blk self.cs,
                    Register.DS => break :blk self.ds,
                    Register.ES => break :blk self.es,
                    Register.SS => break :blk self.ss,
                    else => std.debug.panic("Trying to read an invalid register", .{})
                },
                RegisterHalf.HIGH => switch (regs) {
                    Register.AX => break :blk @intCast((self.ax & 0xFF00) >> 8),
                    Register.BX => break :blk @intCast((self.bx & 0xFF00) >> 8),
                    Register.CX => break :blk @intCast((self.cx & 0xFF00) >> 8),
                    Register.DX => break :blk @intCast((self.dx & 0xFF00) >> 8),
                    else => std.debug.panic("Trying to read an invalid HIGH half register", .{})
                },
                RegisterHalf.LOW => switch (regs) {
                    Register.AX => break :blk @intCast(self.ax & 0x00FF),
                    Register.BX => break :blk @intCast(self.bx & 0x00FF),
                    Register.CX => break :blk @intCast(self.cx & 0x00FF),
                    Register.DX => break :blk @intCast(self.dx & 0x00FF),
                    else => std.debug.panic("Trying to read an invalid LOW half register", .{})
                },
            }
        };
    }

    pub fn write_reg(self: *Cpu, regs: Register, half: RegisterHalf, val: u16) void {
        switch (half) {
            RegisterHalf.FULL => switch (regs) {
                Register.AX => self.ax = val,
                Register.BX => self.bx = val,
                Register.CX => self.cx = val,
                Register.DX => self.dx = val,
                Register.SI => self.si = val,
                Register.DI => self.di = val,
                Register.SP => self.sp = val,
                Register.BP => self.bp = val,
                Register.CS => self.cs = val,
                Register.DS => self.ds = val,
                Register.ES => self.es = val,
                Register.SS => self.ss = val,
                else => std.debug.panic("Trying to read to an invalid register", .{})
            },
            RegisterHalf.HIGH => switch (regs) {
                Register.AX => self.ax = val & 0xFF00,
                Register.BX => self.bx = val & 0xFF00,
                Register.CX => self.cx = val & 0xFF00,
                Register.DX => self.dx = val & 0xFF00,
                else => std.debug.panic("Trying to read to an invalid HIGH half register", .{})
            },
            RegisterHalf.LOW => switch (regs) {
                Register.AX => self.ax = val & 0x00FF,
                Register.BX => self.bx = val & 0x00FF,
                Register.CX => self.cx = val & 0x00FF,
                Register.DX => self.dx = val & 0x00FF,
                else => std.debug.panic("Trying to read to an invalid LOW half register", .{})
            },
        }
    }
};

pub const Emulator = struct {
    cpu: Cpu,

    pub fn init() !Emulator { 
        var emu = std.mem.zeroes(Emulator);
        emu.cpu = try Cpu.init(0);
        return emu;
    }
};

pub fn main() !void {
    //const allocator = std.heap.PageAllocator;
    var emu = try Emulator.init();
    var cpu = emu.cpu;
    //_ = cpu;
    cpu.write_reg(Register.AX, RegisterHalf.FULL, 0xd0f0);
    std.debug.print("{x}", .{cpu.read_reg(Register.AX, RegisterHalf.HIGH)});
}
