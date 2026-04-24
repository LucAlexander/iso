const std = @import("std");
const Buffer = std.ArrayList;
const Map = std.StringHashMap;

const TOKEN = u64;
const open_quote = '(';
const close_quote = ')';
const open_word = ':';
const close_word = ';';
const string_delimiter = '"';
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
			'-' => {
				while (i < text.len){
					if (text[i] == '\n'){
						i += 1;
						break;
					}
					i += 1;
				}
				continue;
			},
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
			string_delimiter => {
				i += 1;
				const start = i;
				while (i < text.len){
					if (text[i] == string_delimiter){
						break;
					}
					i += 1;
				}
				tokens.append(Token{
					.tag = c,
					.value = .{
						.text = text[start..i]
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
	jmp_nc: Word,
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
	eq,
	halt,
	interrupt,
	csh,
	cop,
	ptr,
	ref,
	cpt,
	csz,
	crd,
	cwr,
	cex,
	cin,
	lt,
	gt,
	ge,
	le,
	if_,
	mod
};

const ParseError = error {
	UnexpectedToken,
};

pub fn parse(mem: *const std.mem.Allocator, tokens: []Token, k: u64, instructions: *Buffer(Inst), close_token: ?TOKEN, defs: *Map(Word), def_backlog: *Map(Buffer(u64))) ParseError!u64 {
	var i = k;
	while (i < tokens.len){
		switch (tokens[i].tag){
			string_delimiter => {
				var j: u64 = tokens[i].value.text.len;
				while (j > 0){
					const c = tokens[i].value.text[j-1];
					instructions.append(Inst{ .psh_ds=c}) catch unreachable;
					j -= 1;
				}
				instructions.append(Inst{ .psh_ds=@truncate(tokens[i].value.text.len)}) catch unreachable;
				i += 1;
				continue;
			},
			open_quote => {
				i += 1;
				const save = instructions.items.len;
				instructions.append(Inst{.jmp_nc=0}) catch unreachable;
				const value:Word = @truncate(instructions.items.len*2);
				i = try parse(mem, tokens, i, instructions, close_quote, defs, def_backlog);
				instructions.append(Inst{ .pop_rs=undefined}) catch unreachable;
				instructions.items[save].jmp_nc = @intCast(instructions.items.len*2);
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
				instructions.append(Inst{.jmp_nc=0}) catch unreachable;
				const loc:Word = @intCast(instructions.items.len*2);
				i = try parse(mem, tokens, i, instructions, close_word, defs, def_backlog);
				instructions.append(Inst{ .pop_rs=undefined}) catch unreachable;
				defs.put(name.value.text, loc) catch unreachable;
				instructions.items[save].jmp_nc = @intCast(instructions.items.len*2);
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
				if (std.mem.eql(u8, tokens[i].value.text, "mod")){
					instructions.append(Inst{ .mod = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "if")){
					instructions.append(Inst{ .if_ = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "lt")){
					instructions.append(Inst{ .lt = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "le")){
					instructions.append(Inst{ .le = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "gt")){
					instructions.append(Inst{ .gt = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "ge")){
					instructions.append(Inst{ .ge = undefined }) catch unreachable;
					i += 1;
					continue;
				}
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
				if (std.mem.eql(u8, tokens[i].value.text, "eq")){
					instructions.append(Inst{ .eq = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "int")){
					instructions.append(Inst{ .interrupt = undefined }) catch unreachable;
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
				if (std.mem.eql(u8, tokens[i].value.text, "ref")){
					instructions.append(Inst{ .ref = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "ptr")){
					instructions.append(Inst{ .ptr = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "cpt")){
					instructions.append(Inst{ .cpt = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "csz")){
					instructions.append(Inst{ .csz = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "crd")){
					instructions.append(Inst{ .crd = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "cwr")){
					instructions.append(Inst{ .cwr = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "cex")){
					instructions.append(Inst{ .cex = undefined }) catch unreachable;
					i += 1;
					continue;
				}
				if (std.mem.eql(u8, tokens[i].value.text, "cin")){
					instructions.append(Inst{ .cin = undefined }) catch unreachable;
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
const EQ = 3;
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
const INT = 22;
const CPT = 23;
const CSZ = 24;
const CRD = 25;
const CWR = 26;
const CEX = 27;
const CIN = 28;
const LT = 29;
const LE = 30;
const GT = 31;
const GE = 32;
const IF = 33;
const MOD = 34;
const REF = 35;

const PSH_MASK = 0;
const JMP_MASK = 1;
const INTRINSIC_MASK = 2;
const JMP_NC_MASK = 3;

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
			.jmp_nc => {
				bytes[i] = (@as(u8, @truncate((inst.jmp_nc & 0xFF00) >> 8)) & 0b00111111) | (JMP_NC_MASK << 6);
				i += 1;
				bytes[i] = @truncate(inst.jmp_nc & 0xFF);
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
			.eq => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = EQ;
				i += 1;
			},
			.lt => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = LT;
				i += 1;
			},
			.le => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = LE;
				i += 1;
			},
			.gt => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = GT;
				i += 1;
			},
			.ge => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = GE;
				i += 1;
			},
			.if_ => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = IF;
				i += 1;
			},
			.mod => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = MOD;
				i += 1;
			},
			.halt => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = HLT;
				i += 1;
			},
			.interrupt => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = INT;
				i += 1;
			},
			.ref => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = REF;
				i += 1;
			},
			.ptr => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = PTR;
				i += 1;
			},
			.cpt => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = CPT;
				i += 1;
			},
			.csz => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = CSZ;
				i += 1;
			},
			.crd => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = CRD;
				i += 1;
			},
			.cwr => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = CWR;
				i += 1;
			},
			.cex => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = CEX;
				i += 1;
			},
			.cin => {
				bytes[i] = INTRINSIC_MASK << 6;
				i += 1;
				bytes[i] = CIN;
				i += 1;
			}
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

const CAP_READ = 1;
const CAP_WRITE = 2;
const CAP_EXECUTE = 4;
const CAP_TRAP = 5;

const Cap = struct {
	perms: u8,
	ptr: Word,
	len: Word,

	pub fn allow_read(self: *const Cap, address: Word, hp_start: Word, hp_end: Word) bool {
		if (address >= hp_start and address < hp_end){
			return true;
		}
		if (self.perms & CAP_READ != 0) {
			return address >= self.ptr and address < self.ptr + self.len;
		}
		return false;
	}

	pub fn allow_write(self: *const Cap, address: Word, hp_start: Word, hp_end: Word) bool {
		if (address >= hp_start and address < hp_end){
			return false;
		}
		if (self.perms & CAP_WRITE != 0) {
			return address >= self.ptr and address < self.ptr + self.len;
		}
		return false;
	}

	pub fn allow_execute(self: *const Cap, address: Word, hp_start: Word, hp_end: Word) bool {
		if (address >= hp_start and address < hp_end){
			return false;
		}
		if (self.perms & CAP_EXECUTE != 0) {
			return address >= self.ptr and address < self.ptr + self.len;
		}
		return false;
	}

	pub fn allow_trap(self: *const Cap, address: Word, hp_start: Word, hp_end: Word) bool {
		if (address >= hp_start and address < hp_end){
			return false;
		}
		if (self.perms & CAP_TRAP != 0) {
			return address >= self.ptr and address < self.ptr + self.len;
		}
		return false;
	}
};

pub fn Machine(
	comptime CORES: u8,
	comptime DEVICES: u8,
	comptime DEVICE_LEN: u8,
	comptime int0: fn(*Stack, u8, u8, []u8) void ,
	comptime int1: fn(*Stack, u8, u8, []u8) void ,
	comptime int2: fn(*Stack, u8, u8, []u8) void ,
	comptime int3: fn(*Stack, u8, u8, []u8) void 
) type {
	return struct {
		const Self = @This();
		mem: []u8,
		cap: []Cap,
		ds: [CORES]Stack,
		cs: [CORES]CapStack,
		rs: [CORES]Stack,
		ip: [CORES]Word,
		dev: [DEVICES][] u8,
		dev_busy: [DEVICES] bool,
		hp: Word,
		hp_start: Word,
		hp_end: Word,
		running: [CORES]bool,

		pub fn init(mem: *const std.mem.Allocator, size: Word, ss: Word, css: Word) Self {
			var mach = Self{
				.mem = mem.alloc(u8, size) catch unreachable,
				.cap = mem.alloc(Cap, size/2) catch unreachable,
				.ds = undefined,
				.cs = undefined,
				.rs = undefined,
				.ip = undefined,
				.dev = undefined,
				.dev_busy = undefined,
				.hp = 0,
				.hp_start = 0,
				.hp_end = 0,
				.running = undefined
			};
			for (0..DEVICES) |i| {
				mach.dev_busy[i] = false;
			}
			for (0..size/2) |i| {
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
			const offset:Word = DEVICES*DEVICE_LEN;
			self.hp_end = @as(Word, @truncate(self.mem.len))-offset;
			for (0..CORES) |core| {
				self.cs[core].push(Cap{
					.perms = CAP_READ | CAP_WRITE | CAP_EXECUTE | CAP_TRAP,
					.ptr = loc,
					.len = @truncate(self.mem.len)
				});
			}
		}

		pub fn signal(self: *Self, bytes: []u8) void {
			if (bytes.len > DEVICE_LEN){
				return;
			}
			var offset = self.hp_end;
			var k: u64 = 0;
			for (self.dev_busy) |busy| {
				if (busy){
					k += 1;
					offset += DEVICE_LEN;
					continue;
				}
				break;
			}
			for (offset..offset+DEVICE_LEN) |i| {
				self.mem[i] = bytes[i-offset];
			}
			for (0..CORES) |i| {
				if (self.running[i] == false){
					self.dev_busy[k] = true;
					self.running[i] = true;
					self.ip[i] = offset;
					return;
				}
			}
		}

		pub fn publish_capability(self: *Self, loc: Word, cap: Cap) void {
			self.cap[loc] = cap;
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
						if (address < self.mem.len and cap.allow_write(address, self.hp_start, self.hp_end)){
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
						self.ds[core].push(b);
						self.ds[core].push(a);
						self.ds[core].push(b);
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
						const loc = self.ds[core].pop() & 0x3fff;
						if (loc < self.mem.len){
							if (loc%2 == 0){
								self.rs[core].push(self.ip[core]+2);
								self.ip[core] = loc;
								return;
							}
						}
						self.ds[core].push(loc);
					},
					MOD => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						const c = b % a;
						self.ds[core].push(c);
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
						const c = b -% a;
						self.ds[core].push(c);
					},
					DIV => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						const c = b / a;
						self.ds[core].push(c);
					},
					QUT => {
						const target = self.ds[core].pop();
						if (self.hp+4 > self.hp_end){
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
						const cap = self.cs[core].top();
						const address = self.ds[core].pop();
						if (cap.allow_read(address, self.hp_start, self.hp_end)){
							if (address/2 < self.cap[core].len){
								const data = self.cap[address/2];
								self.cs[core].push(data);
								self.ip[core] += 2;
								return;
							}
						}
						self.ds[core].push(address);
					},
					COP => {
						const cap = self.cs[core].pop();
						const ptr = self.ds[core].pop();
						self.cap[ptr] = cap;
					},
					CAT => {
						const right = self.ds[core].pop();
						const left = self.ds[core].pop();
						if (self.hp+14 > self.hp_end){
							self.hp = self.hp_start;
						}
						const save = self.hp;
						self.mem[self.hp] = @truncate(left>>8);
						self.mem[self.hp] |= (PSH_MASK << 6);
						self.hp += 1;
						self.mem[self.hp] = @truncate(left & 0xff);
						self.hp += 1;
						self.mem[self.hp] = (INTRINSIC_MASK << 6);
						self.hp += 1;
						self.mem[self.hp] = UNQ;
						self.hp += 1;
						self.mem[self.hp] = @truncate(right>>8);
						self.mem[self.hp] |= (PSH_MASK << 6);
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
					EQ => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						if (a == b){
							self.ds[core].push(1);
						}
						else{
							self.ds[core].push(0);
						}
					},
					LT => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						if (a > b){
							self.ds[core].push(1);
						}
						else{
							self.ds[core].push(0);
						}
					},
					LE => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						if (a >= b){
							self.ds[core].push(1);
						}
						else{
							self.ds[core].push(0);
						}
					},
					GT => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						if (a < b){
							self.ds[core].push(1);
						}
						else{
							self.ds[core].push(0);
						}
					},
					GE => {
						const a = self.ds[core].pop();
						const b = self.ds[core].pop();
						if (a <= b){
							self.ds[core].push(1);
						}
						else{
							self.ds[core].push(0);
						}
					},
					IF => {
						const a = self.ds[core].pop();
						if (a == 0){
							const target = self.ds[core].pop();
							_ = self.ds[core].pop();
							self.ds[core].push(target);
						}
						else{
							_ = self.ds[core].pop();
						}
					},
					INT => {
						const cap = self.cs[core].top();
						if (cap.allow_trap(self.ip[core], self.hp_start, self.hp_end)){
							const val = self.ds[core].top();
							const offset = DEVICE_LEN*DEVICES;
							switch (val){
								0 => {
									int0(&self.ds[core], DEVICES, DEVICE_LEN, self.mem[self.mem.len-offset..self.mem.len]);
								},
								1 => {
									int1(&self.ds[core], DEVICES, DEVICE_LEN, self.mem[self.mem.len-offset..self.mem.len]);
								},
								2 => {
									int2(&self.ds[core], DEVICES, DEVICE_LEN, self.mem[self.mem.len-offset..self.mem.len]);
								},
								3 => {
									int3(&self.ds[core], DEVICES, DEVICE_LEN, self.mem[self.mem.len-offset..self.mem.len]);
								},
								else => {}
							}
						}
					},
					HLT => {
						self.running[core] = false;
					},
					REF => {
						const cap = self.cs[core].top();
						const loc = self.ds[core].pop();
						if (loc < self.mem.len and cap.allow_read(loc, self.hp_start, self.hp_end)){
							const data = ((@as(Word, @intCast(self.mem[loc])) << 8) + self.mem[loc + 1]);
							self.ds[core].push(data);
							self.ip[core] += 2;
							return;
						}
						self.ds[core].push(loc);
					},
					PTR => {
						const cap = self.cs[core].top();
						const loc = self.ds[core].pop();
						if (loc < self.mem.len and cap.allow_read(loc, self.hp_start, self.hp_end)){
							const data = ((@as(Word, @intCast(self.mem[loc])) << 8) + self.mem[loc + 1]) & ~long_mask_mask;
							self.ds[core].push(data);
							self.ip[core] += 2;
							return;
						}
						self.ds[core].push(loc);
					},
					CPT => {
						const cap = self.cs[core].top();
						self.ds[core].push(cap.ptr);
					},
					CSZ => {
						const cap = self.cs[core].top();
						self.ds[core].push(cap.len);
					},
					CRD => {
						const cap = self.cs[core].top();
						if (cap.perms & CAP_READ != 0){
							self.ds[core].push(1);
						}
						else{
							self.ds[core].push(0);
						}
					},
					CWR => {
						const cap = self.cs[core].top();
						if (cap.perms & CAP_WRITE != 0){
							self.ds[core].push(1);
						}
						else{
							self.ds[core].push(0);
						}
					},
					CEX => {
						const cap = self.cs[core].top();
						if (cap.perms & CAP_EXECUTE != 0){
							self.ds[core].push(1);
						}
						else{
							self.ds[core].push(0);
						}
					},
					CIN => {
						const cap = self.cs[core].top();
						if (cap.perms & CAP_TRAP != 0){
							self.ds[core].push(1);
						}
						else{
							self.ds[core].push(0);
						}
					},
					else => { }
				}
			}
			else if (mask == PSH_MASK){
				const cap = self.cs[core].top();
				if (cap.allow_read(self.ip[core], self.hp_start, self.hp_end)){
					const data = ((@as(Word, @intCast(self.mem[self.ip[core]])) << 8) + self.mem[self.ip[core] + 1]) & ~long_mask_mask;
					self.ds[core].push(data);
				}
			}
			else if (mask == JMP_MASK){
				self.rs[core].push(self.ip[core]+2);
				const cap = self.cs[core].top();
				const data = ((@as(Word, @intCast(self.mem[self.ip[core]])) << 8) + self.mem[self.ip[core] + 1]) & ~long_mask_mask;
				if (data%2==0 and cap.allow_execute(data, self.hp_start, self.hp_end)){
					self.ip[core] = data;
					return;
				}
				_ = self.rs[core].pop();
			}
			else if (mask == JMP_NC_MASK){
				const cap = self.cs[core].top();
				const data = ((@as(Word, @intCast(self.mem[self.ip[core]])) << 8) + self.mem[self.ip[core] + 1]) & ~long_mask_mask;
				if (data%2==0 and cap.allow_execute(data, self.hp_start, self.hp_end)){
					self.ip[core] = data;
					return;
				}
			}
			self.ip[core] += 2;
		}

		pub fn debugger(self: *Self, mem: *const std.mem.Allocator, fixed: *std.heap.FixedBufferAllocator, core: u8) void {
			const stdout = std.io.getStdOut().writer();
			while (self.running[core]){
				stdout.print("\x1b[2J\x1b[H", .{}) catch unreachable;
				var i: u64 = 0;
				outer: for (0..32) |_| {
					stdout.print("                                                        ", .{}) catch unreachable;
					for (0..12) |_| {
						const block = (@as(Word, @intCast(self.mem[i])) << 8) + self.mem[i+1];
						stdout.print("{x:04} ", .{block}) catch unreachable;
						i += 2;
						if (i > self.mem.len-1){
							break :outer;
						}
					}
					stdout.print("\n", .{}) catch unreachable;
				}
				stdout.print("\x1b[H", .{}) catch unreachable;
				const cs_rows = @min(32, self.cs[core].cursor);
				for (0..cs_rows) |k| {
					stdout.print("                                         ", .{}) catch unreachable;
					const cap = self.cs[core].data[(self.cs[core].cursor-1)-k];
					if (cap.perms & CAP_READ != 0){
						stdout.print("r", .{}) catch unreachable;
					}
					else{
						stdout.print("-", .{}) catch unreachable;
					}
					if (cap.perms & CAP_WRITE != 0){
						stdout.print("w", .{}) catch unreachable;
					}
					else{
						stdout.print("-", .{}) catch unreachable;
					}
					if (cap.perms & CAP_EXECUTE != 0){
						stdout.print("x", .{}) catch unreachable;
					}
					else{
						stdout.print("-", .{}) catch unreachable;
					}
					if (cap.perms & CAP_TRAP != 0){
						stdout.print("t", .{}) catch unreachable;
					}
					else{
						stdout.print("-", .{}) catch unreachable;
					}
					stdout.print(" {x:04} {x:04}\n", .{cap.ptr, cap.len}) catch unreachable;
				}
				stdout.print("\x1b[H", .{}) catch unreachable;
				const rs_rows = @min(32, self.rs[core].cursor);
				for (0..rs_rows) |k| {
					stdout.print("                                    ", .{}) catch unreachable;
					stdout.print("{x:04}\n", .{self.rs[core].data[(self.rs[core].cursor-1)-k]}) catch unreachable;
				}
				stdout.print("\x1b[H", .{}) catch unreachable;
				const ds_rows = @min(32, self.ds[core].cursor);
				for (0..ds_rows) |k| {
					stdout.print("                               ", .{}) catch unreachable;
					stdout.print("{x:04}\n", .{self.ds[core].data[(self.ds[core].cursor-1)-k]}) catch unreachable;
				}
				stdout.print("\x1b[H", .{}) catch unreachable;
				const ip_signed: i64 = self.ip[core];
				const ip_offset:u64 = @max(0, ip_signed-32);
				var k: u64 = ip_offset;
				while (k < ip_offset+64) {
					if (k >= self.mem.len){
						break;
					}
					stdout.print("                          ", .{}) catch unreachable;
					const val = (@as(Word, @intCast(self.mem[k])) << 8) + self.mem[k+1];
					if (k == self.ip[core]){
						stdout.print("\x1b[1;44m{x:04}\x1b[0m\n", .{val}) catch unreachable;
					}
					else{
						stdout.print("{x:04}\n", .{val}) catch unreachable;
					}
					k += 2;
				}
				stdout.print("\x1b[H", .{}) catch unreachable;
				const end_offset = @min(ip_offset+64, self.mem.len);
				if (ip_offset < self.mem.len){
					const byte_slice = self.mem[ip_offset..end_offset];
					const text = disassemble(mem, byte_slice);
					stdout.print("{s}", .{text}) catch unreachable;
					fixed.reset();
				}
				var stdin = std.io.getStdIn().reader();
				var buffer: [1]u8 = undefined;
				_ = stdin.read(&buffer) catch unreachable;
				self.step_core(core);
			}
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

pub fn disassemble(mem: *const std.mem.Allocator, bytes: []u8) []u8 {
	var text = Buffer(u8).init(mem.*);
	var i: u64 = 0;
	const mask:u8 = (3<<6);
	while (i < bytes.len){
		const head = bytes[i];
		const body = bytes[i+1];
		switch ((head & mask)>>6){
			PSH_MASK => {
				const buffer: []u8 = mem.alloc(u8, 7) catch unreachable;
				const slice = std.fmt.bufPrint(buffer, "0x{x:04}\n", .{(@as(Word, @intCast(head)) << 8) + body}) catch unreachable;
				text.appendSlice(slice) catch unreachable;
			},
			JMP_MASK => {
				const buffer: []u8 = mem.alloc(u8, 7) catch unreachable;
				const slice = std.fmt.bufPrint(buffer, "[{x:04}]\n", .{(@as(Word, @intCast(head & ~mask)) << 8) + body}) catch unreachable;
				text.appendSlice(slice) catch unreachable;
			},
			JMP_NC_MASK => {
				const buffer: []u8 = mem.alloc(u8, 7) catch unreachable;
				const slice = std.fmt.bufPrint(buffer, "<{x:04}>\n", .{(@as(Word, @intCast(head & ~mask)) << 8) + body}) catch unreachable;
				text.appendSlice(slice) catch unreachable;
			},
			INTRINSIC_MASK => {
				switch (body){
					NOP => {text.appendSlice("nop\n") catch unreachable;},
					HLT => {text.appendSlice("hlt\n") catch unreachable;},
					POP_DS => {text.appendSlice("pop\n") catch unreachable;},
					EQ => {text.appendSlice("eq\n") catch unreachable;},
					LT => {text.appendSlice("lt\n") catch unreachable;},
					LE => {text.appendSlice("le\n") catch unreachable;},
					GT => {text.appendSlice("gt\n") catch unreachable;},
					GE => {text.appendSlice("ge\n") catch unreachable;},
					IF => {text.appendSlice("if\n") catch unreachable;},
					POP_RS => {text.appendSlice("ret\n") catch unreachable;},
					MOD => {text.appendSlice("mod\n") catch unreachable;},
					ADD => {text.appendSlice("add\n") catch unreachable;},
					MUL => {text.appendSlice("mul\n") catch unreachable;},
					SUB => {text.appendSlice("sub\n") catch unreachable;},
					DIV => {text.appendSlice("div\n") catch unreachable;},
					PTR => {text.appendSlice("ptr\n") catch unreachable;},
					REF => {text.appendSlice("ref\n") catch unreachable;},
					NIP => {text.appendSlice("nip\n") catch unreachable;},
					ROT => {text.appendSlice("rot\n") catch unreachable;},
					DUP => {text.appendSlice("dup\n") catch unreachable;},
					CUT => {text.appendSlice("cut\n") catch unreachable;},
					OVR => {text.appendSlice("ovr\n") catch unreachable;},
					SWP => {text.appendSlice("swp\n") catch unreachable;},
					STR => {text.appendSlice("str\n") catch unreachable;},
					QUT => {text.appendSlice("qut\n") catch unreachable;},
					UNQ => {text.appendSlice("unq\n") catch unreachable;},
					CAT => {text.appendSlice("cat\n") catch unreachable;},
					CSH => {text.appendSlice("csh\n") catch unreachable;},
					COP => {text.appendSlice("cop\n") catch unreachable;},
					INT => {text.appendSlice("int\n") catch unreachable;},
					CPT => {text.appendSlice("cpt\n") catch unreachable;},
					CSZ => {text.appendSlice("csz\n") catch unreachable;},
					CRD => {text.appendSlice("crd\n") catch unreachable;},
					CWR => {text.appendSlice("cwr\n") catch unreachable;},
					CEX => {text.appendSlice("cex\n") catch unreachable;},
					CIN => {text.appendSlice("cin\n") catch unreachable;},
					else => {
						text.appendSlice("???\n") catch unreachable;
					}
				}
			},
			else => {
				text.appendSlice("????\n") catch unreachable;
			}
		}
		i += 2;
	}
	return text.items;
}

pub fn main() !void {
	const heap = std.heap.page_allocator;
	const main_buffer = heap.alloc(u8, 0x10000) catch unreachable;
	const temp_buffer = heap.alloc(u8, 0x10000) catch unreachable;
	var main_mem_fixed = std.heap.FixedBufferAllocator.init(main_buffer);
	var temp_mem_fixed = std.heap.FixedBufferAllocator.init(temp_buffer);
	var main_mem = main_mem_fixed.allocator();
	var temp_mem = temp_mem_fixed.allocator();
	const args = try std.process.argsAlloc(main_mem);
	if (args.len == 1){
		std.debug.print("-h for help\n", .{});
		return;
	}
	if (std.mem.eql(u8, args[1], "-h")){
		std.debug.print("Help Menu\n", .{});
		std.debug.print("   -h : Show this message\n", .{});
		std.debug.print("   [filename] : evaluate file\n", .{});
		std.debug.print("   -g : Debugger\n", .{});
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
	std.debug.print("\n\n", .{});
	const retext = disassemble(&main_mem, bytes);
	std.debug.print("disassembly:\n{s}\n\n", .{retext});
	var mach = Machine(
		2,
		1,
		64,
		int_print,
		int_nop,
		int_nop,
		int_nop
	).init(
		&main_mem,
		1024, 256, 32
	);
	mach.load_rom(0, bytes);
	mach.run(0, 0);
	if (args.len == 3){
		if (std.mem.eql(u8, args[2], "-g")){
			mach.debugger(&temp_mem, &temp_mem_fixed, 0);
		}
	}
	while (mach.active()) {
		mach.step();
	}
}

pub fn int_print(ds: *Stack, _: u8, _: u8, _: []u8) void {
	_ = ds.pop();
	var len = ds.pop();
	while (len > 0){
		std.debug.print("{c}", .{@as(u8, @truncate(ds.pop()))});
		len -= 1;
	}
}

pub fn int_nop(_: *Stack, _:u8, _:u8, _:[]u8) void {
}
