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
						while (std.ascii.isHex(text[i]) and i < text.len){
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
						while (std.ascii.isDigit(text[i]) and i < text.len){
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
					while ((std.ascii.isAlphanumeric(text[i]) or c == '_') and i < text.len){
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
	psh_ds,
	pop_ds,
	psh_rs,
	pop_rs,
	write_ds,
	jmp,
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
	data: Word
};

const ParseError = error {
	UnexpectedToken,
};

pub fn parse(mem: *const std.mem.Allocator, tokens: []Token, k: u64, instructions: *Buffer(Inst), close_token: ?TOKEN) ParseError!u64 {
	var i = k;
	var defs = Map(Word).init(mem.*);
	var def_backlog = Map(Buffer(u64)).init(mem.*);
	while (i < tokens.len){
		switch (tokens[i].tag){
			open_quote => {
				i += 1;
				instructions.append(Inst{.jmp=undefined}) catch unreachable;
				const save = instructions.items.len;
				instructions.append(Inst{.data=0}) catch unreachable;
				i = try parse(mem, tokens, i, instructions, close_quote);
				instructions.append(Inst{ .pop_rs=undefined}) catch unreachable;
				instructions.items[save].data = @intCast(instructions.items.len*2);
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
				const loc:Word = @intCast(instructions.items.len*2);
				const name = tokens[i];
				if (name.tag != iden){
					return ParseError.UnexpectedToken;
				}
				i += 1;
				instructions.append(Inst{.jmp=undefined}) catch unreachable;
				const save = instructions.items.len;
				instructions.append(Inst{.data=0}) catch unreachable;
				i = try parse(mem, tokens, i, instructions, close_word);
				instructions.append(Inst{ .pop_rs=undefined}) catch unreachable;
				defs.put(name.value.text, loc) catch unreachable;
				instructions.items[save].data = @intCast(instructions.items.len*2);
				if (def_backlog.get(name.value.text)) |list| {
					for (list.items) |index| {
						instructions.items[index].data = loc;
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
				if (defs.get(tokens[i].value.text)) |address| {
					instructions.append(Inst{ .psh_rs = undefined}) catch unreachable;
					instructions.append(Inst{ .jmp=undefined, }) catch unreachable;
					instructions.append(Inst{ .data = address}) catch unreachable;
					i += 1;
					continue;
				}
				instructions.append(Inst{ .psh_rs = undefined}) catch unreachable;
				instructions.append(Inst{ .jmp = undefined, }) catch unreachable;
				instructions.append(Inst{ .data = 0}) catch unreachable;
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
				instructions.append(Inst{ .psh_ds = undefined}) catch unreachable;
				instructions.append(Inst{ .data = tokens[i].value.numeric }) catch unreachable;
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
const PSH_DS = 1;
const POP_DS = 2;
const PSH_RS = 3;
const POP_RS = 4;
const ADD = 5;
const MUL = 6;
const SUB = 7;
const DIV = 8;
const JMP = 9;
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

pub fn code_gen(mem: *const std.mem.Allocator, instructions: Buffer(Inst)) []u8 {
	var bytes = mem.alloc(u8, instructions.items.len*2) catch unreachable;
	var i: u64 = 0;
	for (instructions.items) |inst| {
		switch (inst){
			.psh_ds => {bytes[i] = PSH_DS;},
			.pop_ds => {bytes[i] = POP_DS;},
			.psh_rs => {bytes[i] = PSH_RS;},
			.pop_rs => {bytes[i] = POP_RS;},
			.write_ds => {bytes[i] = STR;},
			.jmp => {bytes[i] = JMP;},
			.nip => {bytes[i] = NIP;},
			.rot => {bytes[i] = ROT;},
			.dup => {bytes[i] = DUP;},
			.cut => {bytes[i] = CUT;},
			.ovr => {bytes[i] = OVR;},
			.swp => {bytes[i] = SWP;},
			.add => {bytes[i] = ADD;},
			.sub => {bytes[i] = SUB;},
			.mul => {bytes[i] = MUL;},
			.div => {bytes[i] = DIV;},
			.cat => {bytes[i] = CAT;},
			.quote => {bytes[i] = QUT;},
			.unquote => {bytes[i] = UNQ;},
			.data => {
				bytes[i] = @truncate((inst.data & 0xFF00) >> 8);
				i += 1;
				bytes[i] = @truncate(inst.data & 0xFF);
				i += 1;
				continue;
			}
		}
		i += 2;
	}
	return bytes;
}

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

const Machine = struct {
	mem: []u8,
	ds: Stack,
	rs: Stack,
	ip: Word,
	hp: Word,
	hp_start: Word,
	running: bool,

	pub fn init(mem: *const std.mem.Allocator, size: Word, ss: Word) Machine {
		return Machine{
			.mem = mem.alloc(u8, size) catch unreachable,
			.ds = Stack.init(mem, ss),
			.rs = Stack.init(mem, ss),
			.ip = 0,
			.hp = 0,
			.hp_start = 0,
			.running = false
		};
	}

	pub fn load_rom(self: *Machine, loc: Word, bytes: []u8) void {
		var i:Word= loc;
		while (i < loc+bytes.len){
			self.mem[i] = bytes[i-loc];
			i += 1;
		}
		self.hp = i;
		self.hp_start = i;
	}

	pub fn run(self: *Machine, ip: Word) void {
		self.ip = ip;
		self.running = true;
	}

	pub fn step(self: *Machine) void {
		if (self.running == false){
			return;
		}
		switch (self.mem[self.ip]) {
			NOP => {},
			PSH_DS => {
				self.ip += 2;
				if (self.ip < self.mem.len){
					const data = (@as(Word, @intCast(self.mem[self.ip])) << 8) + self.mem[self.ip + 1];
					self.ds.push(data);
				}
			},
			POP_DS => {
				_ = self.ds.pop();
			},
			PSH_RS => {
				self.rs.push(self.ip);
			},
			POP_RS => {
				self.ip = self.rs.pop();
			},
			STR => {
				const loc = self.ds.pop();
				if (loc < self.mem.len){
					const address = (@as(Word, @intCast(self.mem[loc])) << 8) + self.mem[loc + 1];
					if (address < self.mem.len){
						if (address % 2 == 0){
							const data = self.ds.pop();
							self.mem[address] = @truncate(data >> 8);
							self.mem[address+1] = @truncate(data & 0xFF);
							self.ip += 2;
							return;
						}
					}
				}
				self.ds.push(loc);
			},
			JMP => {
				self.ip += 2;
				if (self.ip < self.mem.len){
					const data = (@as(Word, @intCast(self.mem[self.ip])) << 8) + self.mem[self.ip + 1];
					if (data%2==0){
						self.ip = data;
						return;
					}
				}
			},
			NIP => {
				const a = self.ds.pop();
				_ = self.ds.pop();
				self.ds.push(a);
			},
			OVR => {
				const a = self.ds.pop();
				const b = self.ds.pop();
				const c = self.ds.pop();
				self.ds.push(b);
				self.ds.push(a);
				self.ds.push(c);
			},
			DUP => {
				const a = self.ds.pop();
				self.ds.push(a);
				self.ds.push(a);
			},
			CUT => {
				const a = self.ds.pop();
				const b = self.ds.pop();
				_ = self.ds.pop();
				self.ds.push(b);
				self.ds.push(a);
			},
			ROT => {
				const a = self.ds.pop();
				const b = self.ds.pop();
				const c = self.ds.pop();
				self.ds.push(a);
				self.ds.push(c);
				self.ds.push(b);
			},
			SWP => {
				const a = self.ds.pop();
				const b = self.ds.pop();
				self.ds.push(a);
				self.ds.push(b);
			},
			UNQ => {
				const loc = self.ds.pop();
				if (loc < self.mem.len){
					const address = (@as(Word, @intCast(self.mem[loc])) << 8) + self.mem[loc + 1];
					if (address < self.mem.len){
						const new_ip = (@as(Word, @intCast(self.mem[address])) << 8) + self.mem[address + 1];
						if (address%2 == 0){
							self.rs.push(self.ip);
							self.ip = new_ip;
							return;
						}
					}
				}
				self.ds.push(loc);
			},
			ADD => {
				const a = self.ds.pop();
				const b = self.ds.pop();
				const c = a +% b;
				self.ds.push(c);
			},
			MUL => {
				const a = self.ds.pop();
				const b = self.ds.pop();
				const c = a *% b;
				self.ds.push(c);
			},
			SUB => {
				const a = self.ds.pop();
				const b = self.ds.pop();
				const c = a -% b;
				self.ds.push(c);
			},
			DIV => {
				const a = self.ds.pop();
				const b = self.ds.pop();
				const c = a / b;
				self.ds.push(c);
			},
			QUT => {
				const target = self.ds.pop();
				if (self.hp+4 > self.mem.len){
					self.hp = self.hp_start;
				}
				const save = self.hp;
				self.mem[self.hp] = @truncate(target>>8);
				self.hp += 1;
				self.mem[self.hp] = @truncate(target & 0xff);
				self.hp += 1;
				self.mem[self.hp] = POP_RS;
				self.hp += 2;
				self.ds.push(save);
			},
			CAT => {
				const right = self.ds.pop();
				const left = self.ds.pop();
				if (self.hp+14 > self.mem.len){
					self.hp = self.hp_start;
				}
				const save = self.hp;
				self.mem[self.hp] = PSH_DS;
				self.hp += 2;
				self.mem[self.hp] = @truncate(left>>8);
				self.hp += 1;
				self.mem[self.hp] = @truncate(left & 0xff);
				self.hp += 1;
				self.mem[self.hp] = UNQ;
				self.hp += 2;
				self.mem[self.hp] = PSH_DS;
				self.hp += 2;
				self.mem[self.hp] = @truncate(right>>8);
				self.hp += 1;
				self.mem[self.hp] = @truncate(right & 0xff);
				self.hp += 1;
				self.mem[self.hp] = UNQ;
				self.hp += 2;
				self.mem[self.hp] = POP_RS;
				self.hp += 2;
				self.ds.push(save);
			},
			else => { }
		}
		self.ip += 2;
	}
};

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
	_ = parse(&main_mem, tokens.items, 0, &instructions, null) catch unreachable;
	const bytes = code_gen(&main_mem, instructions);
	for (bytes) |b| {
		std.debug.print("{x:02} ", .{b});
	}
	std.debug.print("\n", .{});
	var mach = Machine.init(&main_mem, 1024, 256);
	mach.load_rom(0, bytes);
	mach.run(0);
	while (mach.running){
		mach.step();
	}
}


//TODO multicore
