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
	psh_cs,
	pop_cs,
	read_ds,
	write_ds,
	jmp,
	nip,
	rot,
	dup,
	cut,
	ovr,
	swp,
	run,
	add,
	sub,
	mul,
	div,
	data: Word
};

const ParseError = error {
	UnexpectedToken,
};

pub fn parse(mem: *const std.mem.Allocator, tokens: []Token, k: u64, instructions: *Buffer(Inst), close_token: ?TOKEN) ParseError!u64 {
	//TODO
}

const OPCODE = u8;
const NOP = 0;
const PSH_DS = 1;
const POP_DS = 2;
const PSH_RS = 3;
const POP_RS = 4;
const PSH_CS = 5;
const POP_CS = 6;
const LD = 7;
const ST = 8;
const JMP = 9;
const NIP = 10;
const ROT = 11;
const DUP = 12;
const CUT = 13;
const OVR = 14;
const SWP = 15;
const RUN = 16;
const ADD = 17;
const MUL = 18;
const SUB = 19;
const DIV = 20;

pub fn code_gen(mem: *const std.mem.Allocator, instructions: Buffer(Inst)) []u8 {
	//TODO
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
	_ = parse(&main_mem, tokens.items, 0, &instructions, null) catch unreachable;
	const bytes = code_gen(&main_mem, instructions);
	for (bytes) |b| {
		std.debug.print("{x:02} ", .{b});
	}
	std.debug.print("\n", .{});
}


