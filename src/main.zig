const std = @import("std");
const Buffer = std.ArrayList;
const Map = std.StringHashMap;

const TOKEN = u64;
const open_quote = '(';
const close_quote = ')';
const open_word = ':';
const close_word = ';';
const iden = 0;
const number = 1;

const Word = u16;

const Token = struct {
	tag: TOKEN,
	value: union(enum){
		text: []const u8,
		numeric: Word
	}
};

pub fn tokenize(mem: *const std.mem.Allocator, text: []const u8) Buffer(Token) {
	var tokens = Buffer(Token).init(mem.*);
	var i: u64 = 0;
	while (i < text.len){
		const c = text[i];
		switch(c) {
			' ', '\t', '\n', '\r' => {
				i += 1;
				continue;
			},
			open_quote, close_quote, open_word, close_word => {
				tokens.append(Token{
					.tag = c,
					.value = .{
						.text = text[i..i+1]
					}
				}) catch unreachable;
				i += 1;
				continue;
			},
			else => {
				if (std.ascii.isDigit(c)){
					i += 1;
					var value: Word = c-48;
					if (text[i] == 'x'){
						i += 1;
						while (i < text.len and std.ascii.isHex(text[i])){
							value *= 16;
							if (std.ascii.isDigit(text[i])){
								value += text[i]-48;
							}
							else if (std.ascii.isLower(text[i])){
								value += (text[i]-97)+10;
							}
							else {
								value += (text[i]-65)+10;
							}
							i += 1;
						}
					}
					else{
						while (i < text.len and std.ascii.isDigit(text[i])){
							value *= 10;
							value += text[i]-48;
							i += 1;
						}
					}
					tokens.append(Token{
						.tag = number,
						.value = .{
							.numeric = value
						}
					}) catch unreachable;
					continue;
				}
				else if (std.ascii.isAlphanumeric(c) or c == '_'){
					const start = i;
					while (i < text.len and (std.ascii.isAlphanumeric(text[i]) or text[i] == '_')){
						i += 1;
					}
					tokens.append(Token{
						.tag = iden,
						.value = .{
							.text = text[start .. i]
						}
					}) catch unreachable;
					continue;
				}
			}
		}
		i += 1;
	}
	return tokens;
}

const Inst = union(enum){
	psh_ds: Word,
	pop_ds,
	pop_rs,
	write_ds,
	jmp: Word,
	nip,
	rot,
	dup,
	cut,
	ovr,
	swp,
	add,
	sub,
	mul,
	div,
	cat,
	quote,
	unquote,
	eq0,
	halt,
	csh,
	cop,
	ptr,
};

const ParseError = error {
	UnexpectedToken,
};

pub fn parse(mem: *const std.mem.Allocator, tokens: []Token, k: u64, instructions: *Buffer(Inst), close_token: ?TOKEN, defs: *Map(Word), def_backlog: *Map(Buffer(u64))) ParseError!u64 {
	var i = k;
	while (i < tokens.len){
		switch (tokens[i].tag){
			open_quote => {
				i += 1;
				const save = instructions.items.len;
				instructions.append(Inst{.jmp=0}) catch unreachable;
				const value:Word = @truncate(instructions.items.len*2);
				i = try parse(mem, tokens, i, instructions, close_quote, defs, def_backlog);
				instructions.append(Inst{ .pop_rs=undefined}) catch unreachable;
				instructions.items[save].jmp = @intCast(instructions.items.len*2);
				instructions.append(Inst{ .psh_ds=value}) catch unreachable;
				i += 1;
				continue;
			},
			close_quote => {
				if (close_token)|end|{
					if (end == close_quote){
						return i;
					}
				}
				return ParseError.UnexpectedToken;
			},
			open_word => {
				i += 1;
				const name = tokens[i];
				if (name.tag != iden){
					return ParseError.UnexpectedToken;
				}
				i += 1;
				const save = instructions.items.len;
				instructions.append(Inst{.jmp=0}) catch unreachable;
				const loc:Word = @intCast(instructions.items.len*2);
				i = try parse(mem, tokens, i, instructions, close_word, defs, def_backlog);
				instructions.append(Inst{ .pop_rs=undefined}) catch unreachable;
				defs.put(name.value.text, loc) catch unreachable;
				instructions.items[save].jmp= @intCast(instructions.items.len*2);
				if (def_backlog.get(name.value.text)) |list| {
					for (list.items) |index| {
						instructions.items[index].jmp = loc;
					}
				}
				i += 1;
				continue;
			},
			close_word => {
				if (close_token)|end|{
					if (end == close_word){
						return i;
					}
				}
				return ParseError.UnexpectedToken;
			},
			iden => {
				if (std.mem.eql(u8, tokens[i].value.text, "cat")){
					instructions.append(Inst{ .cat = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "qut")){
					instructions.append(Inst{ .quote = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "unq")){
					instructions.append(Inst{ .unquote = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "nip")){
					instructions.append(Inst{ .nip = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "ovr")){
					instructions.append(Inst{ .ovr = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "swp")){
					instructions.append(Inst{ .swp = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "rot")){
					instructions.append(Inst{ .rot = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "dup")){
					instructions.append(Inst{ .dup = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "cut")){
					instructions.append(Inst{ .cut = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "pop")){
					instructions.append(Inst{ .pop_ds = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "str")){
					instructions.append(Inst{ .write_ds = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "add")){
					instructions.append(Inst{ .add = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "sub")){
					instructions.append(Inst{ .sub = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "mul")){
					instructions.append(Inst{ .mul = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "div")){
					instructions.append(Inst{ .div = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "eq0")){
					instructions.append(Inst{ .eq0 = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "hlt")){
					instructions.append(Inst{ .halt = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "csh")){
					instructions.append(Inst{ .csh = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "cop")){
					instructions.append(Inst{ .cop = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "ptr")){
					instructions.append(Inst{ .ptr = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (defs.get(tokens[i].value.text)) |address| {
					instructions.append(Inst{ .jmp=address, }) catch unreachable;
					i += 1;
					continue;
				}
				instructions.append(Inst{ .jmp = 0}) catch unreachable;
				if (def_backlog.getPtr(tokens[i].value.text)) |list| {
					list.append(instructions.items.len-1) catch unreachable;
				}
				else {
					var list = Buffer(u64).init(mem.*);
					list.append(instructions.items.len-1) catch unreachable;
					def_backlog.put(tokens[i].value.text, list) catch unreachable;
				}
				i += 1;
				continue;
			},
			number => {
				instructions.append(Inst{ .psh_ds = tokens[i].value.numeric}) catch unreachable;
				i += 1;
				continue;
			},
			else => {
				return ParseError.UnexpectedToken;
			}
		}
	}
	return i;
}

const OPCODE = u8;
const NOP = 0;
const HLT = 1;
const POP_DS = 2;
const EQ0 = 3;
const POP_RS = 4;
const ADD = 5;
const MUL = 6;
const SUB = 7;
const DIV = 8;
const PTR = 9;
const NIP = 10;
const ROT = 11;
const DUP = 12;
const CUT = 13;
const OVR = 14;
const SWP = 15;
const STR = 16;
const QUT = 17;
const UNQ = 18;
const CAT = 19;
const CSH = 20;
const COP = 21;

const PSH_MASK = 0;
const JMP_MASK = 1;
const INTRINSIC_MASK = 2;

pub fn code_gen(mem: *const std.mem.Allocator, instructions: Buffer(Inst)) []u8 {
	var bytes = mem.alloc(u8, instructions.items.len*2) catch unreachable;
	var i: u64 = 0;
	var k: u64 = 0;
	while (k<instructions.items.len) {
		const inst = instructions.items[k];
		switch (inst){
			.psh_ds => {
				bytes[i] = (@as(u8, @truncate((inst.psh_ds & 0xFF00) >> 8)) & 0b00111111) | (PSH_MASK << 6);
				i += 1;
				bytes[i] = @truncate(inst.psh_ds & 0xFF);
				i += 1;
			},
			.pop_ds => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = POP_DS;
				i += 1;
			},
			.pop_rs => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = POP_RS;
				i += 1;
			},
			.write_ds => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = STR;
				i += 1;
			},
			.jmp => {
				bytes[i] = (@as(u8, @truncate((inst.jmp & 0xFF00) >> 8)) & 0b00111111) | (JMP_MASK << 6);
				i += 1;
				bytes[i] = @truncate(inst.jmp & 0xFF);
				i += 1;
			},
			.nip => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = NIP;
				i += 1;
			},
			.rot => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = ROT;
				i += 1;
			},
			.dup => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = DUP;
				i += 1;
			},
			.cut => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = CUT;
				i += 1;
			},
			.ovr => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = OVR;
				i += 1;
			},
			.swp => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = SWP;
				i += 1;
			},
			.add => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = ADD;
				i += 1;
			},
			.sub => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = SUB;
				i += 1;
			},
			.mul => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = MUL;
				i += 1;
			},
			.div => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = DIV;
				i += 1;
			},
			.cat => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = CAT;
				i += 1;
			},
			.csh => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = CSH;
				i += 1;
			},
			.cop => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = COP;
				i += 1;
			},
			.quote => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = QUT;
				i += 1;
			},
			.unquote => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = UNQ;
				i += 1;
			},
			.eq0 => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = EQ0;
				i += 1;
			},
			.halt => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = HLT;
				i += 1;
			},
			.ptr => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = PTR;
				i += 1;
			},
		}
		k += 1;
	}
	return bytes;
}

const CapStack = struct {
	data: []Cap,
	cursor: Word,

	pub fn init(mem: *const std.mem.Allocator, size: Word) CapStack {
		return CapStack{
			.data = mem.alloc(Cap, size) catch unreachable,
			.cursor = 0
		};
	}

	pub fn push(self: *CapStack, item: Cap) void {
		if (self.cursor == self.data.len){
			self.cursor = 0;
		}
		self.data[self.cursor] = item;
		self.cursor += 1;
	}

	pub fn top(self: *CapStack) Cap {
		if (self.cursor == 0){
			return self.data[self.data.len-1];
		}
		return self.data[self.cursor-1];
	}
	
	pub fn pop(self: *CapStack) Cap {
		if (self.cursor == 0){
			self.cursor = @intCast(self.data.len);
		}
		self.cursor -= 1;
		return self.data[self.cursor];
	}
};

const Stack = struct {
	data: []Word,
	cursor: Word,

	pub fn init(mem: *const std.mem.Allocator, size: Word) Stack {
		return Stack{
			.data = mem.alloc(Word, size) catch unreachable,
			.cursor = 0
		};
	}

	pub fn push(self: *Stack, item: Word) void {
		if (self.cursor == self.data.len){
			self.cursor = 0;
		}
		self.data[self.cursor] = item;
		self.cursor += 1;
	}

	pub fn top(self: *Stack) Word {
		if (self.cursor == 0){
			return self.data[self.data.len-1];
		}
		return self.data[self.cursor-1];
	}
	
	pub fn pop(self: *Stack) Word {
		if (self.cursor == 0){
			self.cursor = @intCast(self.data.len);
		}
		self.cursor -= 1;
		return self.data[self.cursor];
	}
};

const CAP_WRITE = 1;
const CAP_EXECUTE = 2;

const Cap = struct {
	perms: u8,
	ptr: Word,
	len: Word,

	pub fn allow_write(self: *const Cap, address: Word, hp_start: Word) bool {
		if (address > hp_start){
			return false;
		}
		if (self.perms & CAP_WRITE != 0) {
			return address > self.ptr and address < self.ptr + self.len;
		}
		return false;
	}

	pub fn allow_execute(self: *const Cap, address: Word, hp_start: Word) bool {
		if (address > hp_start){
			return false;
		}
		if (self.perms & CAP_EXECUTE != 0) {
			return address > self.ptr and address < self.ptr + self.len;
		}
		return false;
	}
};

pub fn Machine(comptime CORES: u8) type {
	return struct {
		const Self = @This();
		mem: []u8,
		cap: []Cap,
		ds: [CORES]Stack,
		cs: [CORES]CapStack,
		rs: [CORES]Stack,
		ip: [CORES]Word,
		hp: Word,
		hp_start: Word,
		running: [CORES]bool,

		pub fn init(mem: *const std.mem.Allocator, size: Word, ss: Word, css: Word) Self {
			var mach = Self{
				.mem = mem.alloc(u8, size) catch unreachable,
				.cap = mem.alloc(Cap, size/2) catch unreachable,
				.ds = undefined,
				.cs = undefined,
				.rs = undefined,
				.ip = undefined,
				.hp = 0,
				.hp_start = 0,
				.running = undefined
			};
			for (0..size) |i| {
				mach.cap[i] = Cap{
					.perms=0,
					.ptr = 0,
					.len = 0
				};
			}
			for (0..CORES) |i| {
				mach.ds[i] = Stack.init(mem, ss);
				mach.cs[i] = CapStack.init(mem, css);
				mach.rs[i] = Stack.init(mem, ss);
				mach.ip[i] = 0;
				mach.running[i] = false;
			}
			return mach;
		}

		pub fn load_rom(self: *Self, loc: Word, bytes: []u8) void {
			var i:Word= loc;
			while (i < loc+bytes.len){
				self.mem[i] = bytes[i-loc];
				i += 1;
			}
			self.hp = i;
			self.hp_start = i;
		}

		pub fn run(self: *Self, core: u8, ip: Word) void {
			self.ip[core] = ip;
			self.running[core] = true;
		}
		
		pub fn active(self: *Self) bool {
			for (0..CORES)|i|{
				if (self.running[i]){
					return true;
				}
			}
			return false;
		}

		pub fn step(self: *Self) void {
			for (0..CORES)|i|{
				self.step_core(@truncate(i));
			}
		}

		pub fn step_core(self: *Self, core: u8) void {
			if (self.running[core] == false){
				return;
			}
			const inst:u8 = self.mem[self.ip[core]];
			const mask_mask:u8 = 3<<6;
			const long_mask_mask:Word = 3<<14;
			const mask:u8 = (inst & mask_mask) >> 6;
			std.debug.print("{x:02} {x:02} ", .{self.mem[self.ip[core]], self.mem[self.ip[core]+1]});
			if (mask == INTRINSIC_MASK){
				const opcode = self.mem[self.ip[core]+1];
				switch (opcode) {
					NOP => {},
					POP_DS => {
						_ = self.ds[core].pop();
					},
					POP_RS => {
						self.ip[core] = self.rs[core].pop();
						return;
					},
					STR => {
						const cap = self.cs[core].top();
						const address = self.ds[core].pop();
						if (address < self.mem.len and cap.allow_write(address, self.hp_start)){
							if (address % 2 == 0){
								const data = self.ds[core].pop();
								self.mem[address] = @truncate(data >> 8);
								self.mem[address+1] = @truncate(data & 0xFF);
								self.ip[core] += 2;
								return;
							}
						}
						self.ds[core].push(address);
					},
					NIP => {
						const a = self.ds[core].pop();
						_ = self.ds[core].pop();
						self.ds[core].push(a);
					},
					OVR => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						const c = self.ds[core].pop();
						self.ds[core].push(b);
						self.ds[core].push(a);
						self.ds[core].push(c);
					},
					DUP => {
						const a = self.ds[core].pop();
						self.ds[core].push(a);
						self.ds[core].push(a);
					},
					CUT => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						_ = self.ds[core].pop();
						self.ds[core].push(b);
						self.ds[core].push(a);
					},
					ROT => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						const c = self.ds[core].pop();
						self.ds[core].push(a);
						self.ds[core].push(c);
						self.ds[core].push(b);
					},
					SWP => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						self.ds[core].push(a);
						self.ds[core].push(b);
					},
					UNQ => {
						const loc = self.ds[core].pop();
						if (loc < self.mem.len){
							if (loc%2 == 0){
								const new_ip = (@as(Word, @intCast(self.mem[loc])) << 8) + self.mem[loc + 1];
								self.rs[core].push(self.ip[core]+2);
								self.ip[core] = new_ip;
								return;
							}
						}
						self.ds[core].push(loc);
					},
					ADD => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						const c = a +% b;
						self.ds[core].push(c);
					},
					MUL => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						const c = a *% b;
						self.ds[core].push(c);
					},
					SUB => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						const c = a -% b;
						self.ds[core].push(c);
					},
					DIV => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						const c = a / b;
						self.ds[core].push(c);
					},
					QUT => {
						const target = self.ds[core].pop();
						if (self.hp+4 > self.mem.len){
							self.hp = self.hp_start;
						}
						const save = self.hp;
						self.mem[self.hp] = @truncate(target>>8);
						self.hp += 1;
						self.mem[self.hp] = @truncate(target & 0xff);
						self.hp += 1;
						self.mem[self.hp] = (INTRINSIC_MASK << 6);
						self.hp += 1;
						self.mem[self.hp] = POP_RS;
						self.hp += 1;
						self.ds[core].push(save);
					},
					CSH => {
						const address = self.ds[core].pop();
						if (address/2 < self.cap[core].len){
							const data = self.cap[address/2];
							self.cs[core].push(data);
							self.ip[core] += 2;
							return;
						}
						self.ds[core].push(address);
					},
					COP => {
						_ = self.cs[core].pop();
					},
					CAT => {
						const right = self.ds[core].pop();
						const left = self.ds[core].pop();
						if (self.hp+14 > self.mem.len){
							self.hp = self.hp_start;
						}
						const save = self.hp;
						self.mem[self.hp] = @truncate(left>>8);
						self.mem[self.hp] &= (PSH_MASK << 6);
						self.hp += 1;
						self.mem[self.hp] = @truncate(left & 0xff);
						self.hp += 1;
						self.mem[self.hp] = (INTRINSIC_MASK << 6);
						self.hp += 1;
						self.mem[self.hp] = UNQ;
						self.hp += 1;
						self.mem[self.hp] = @truncate(right>>8);
						self.mem[self.hp] &= (PSH_MASK << 6);
						self.hp += 1;
						self.mem[self.hp] = @truncate(right & 0xff);
						self.hp += 1;
						self.mem[self.hp] = (INTRINSIC_MASK << 6);
						self.hp += 1;
						self.mem[self.hp] = UNQ;
						self.hp += 1;
						self.mem[self.hp] = (INTRINSIC_MASK << 6);
						self.hp += 1;
						self.mem[self.hp] = POP_RS;
						self.hp += 1;
						self.ds[core].push(save);
					},
					EQ0 => {
						const val = self.ds[core].pop();
						if (val == 0){
							_ = self.ds[core].pop();
						}
						else{
							const target = self.ds[core].pop();
							_ = self.ds[core].pop();
							self.ds[core].push(target);
						}
					},
					HLT => {
						self.running[core] = false;
					},
					PTR => {
						const loc = self.ds[core].pop();
						if (loc < self.mem.len){
							const data = ((@as(Word, @intCast(self.mem[loc])) << 8) + self.mem[loc + 1]) & ~long_mask_mask;
							self.ds[core].push(data);
							self.ip[core] += 2;
							return;
						}
						self.ds[core].push(loc);
					},
					else => { }
				}
			}
			else if (mask == PSH_MASK){
				const data = ((@as(Word, @intCast(self.mem[self.ip[core]])) << 8) + self.mem[self.ip[core] + 1]) & ~long_mask_mask;
				self.ds[core].push(data);
			}
			else if (mask == JMP_MASK){
				self.rs[core].push(self.ip[core]+2);
				const cap = self.cs[core].top();
				const data = ((@as(Word, @intCast(self.mem[self.ip[core]])) << 8) + self.mem[self.ip[core] + 1]) & ~long_mask_mask;
				if (data%2==0 and cap.allow_execute(data, self.hp_start)){
					self.ip[core] = data;
					return;
				}
				_ = self.rs[core].pop();
			}
			self.ip[core] += 2;
		}
	};
}

pub fn get_contents(mem: *const std.mem.Allocator, filename: []const u8) ![]u8 {
	var infile = std.fs.cwd().openFile(filename, .{}) catch |err| {
		std.debug.print("File not found: {s}\n", .{filename});
		return err;
	};
	defer infile.close();
	const stat = infile.stat() catch |err| {
		std.debug.print("Errored file stat: {s}\n", .{filename});
		return err;
	};
	const contents = infile.readToEndAlloc(mem.*, stat.size+1) catch |err| {
		std.debug.print("Error reading file: {s}\n", .{filename});
		return err;
	};
	return contents;
}

pub fn main() !void {
	const heap = std.heap.page_allocator;
	const main_buffer = heap.alloc(u8, 0x10000) catch unreachable;
	const temp_buffer = heap.alloc(u8, 0x10000) catch unreachable;
	var main_mem_fixed = std.heap.FixedBufferAllocator.init(main_buffer);
	var temp_mem_fixed = std.heap.FixedBufferAllocator.init(temp_buffer);
	var main_mem = main_mem_fixed.allocator();
	_ = temp_mem_fixed.allocator();
	const args = try std.process.argsAlloc(main_mem);
	if (args.len == 1){
		std.debug.print("-h for help\n", .{});
		return;
	}
	if (std.mem.eql(u8, args[1], "-h")){
		std.debug.print("Help Menu\n", .{});
		std.debug.print("   -h : Show this message\n", .{});
		std.debug.print("   [filename] : evaluate file\n", .{});
		return;
	}
	const filename = args[1];
	const contents = try get_contents(&main_mem, filename);
	const tokens = tokenize(&main_mem, contents);
	var instructions = Buffer(Inst).init(main_mem);
	var defs = Map(Word).init(main_mem);
	var def_backlog = Map(Buffer(u64)).init(main_mem);
	_ = parse(&main_mem, tokens.items, 0, &instructions, null, &defs, &def_backlog) catch unreachable;
	const bytes = code_gen(&main_mem, instructions);
	for (bytes) |b| {
		std.debug.print("{x:02} ", .{b});
	}
	std.debug.print("\n", .{});
	var mach = Machine(2).init(&main_mem, 1024, 256, 32);
	mach.load_rom(0, bytes);
	mach.run(0, 0);
	while (mach.active()) {
		mach.step();
	}
}

//TODO
	//capabilities
	//devices
	//discrete comptime interrupt instructions
