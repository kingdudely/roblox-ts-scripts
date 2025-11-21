function getn(t: unknown) {
	throw 'make getn return #t';
	return 0;
}

function toSigned(x: number) {
	return x >= 0x80000000 ? x - 0x100000000 : x;
}

function toBuffer(x: TypedArray) {
	const result = buffer.create(x.byteLength);
	buffer.copy(result, 0, x.buffer, x.byteOffset, x.byteLength);
	return result;
}


// Some numerical data is initialized as -1 even when it doesn't need initialization to help the JIT infer types
type TypedArraySource = number | buffer | TypedArray | Array < number > ;

const bufferBySize: {
	[index: number]: Array < [Callback, Callback] >
} = {
	1: [
		[buffer.readu8, buffer.writeu8],
		[buffer.readi8, buffer.writei8],
	],
	2: [
		[buffer.readu16, buffer.writeu16],
		[buffer.readi16, buffer.writei16],
	],
	4: [
		[buffer.readu32, buffer.writeu32],
		[buffer.readi32, buffer.writei32],
	],
};

// ---- TypedArray base class ----
class TypedArray {
	public buffer: buffer;
	public byteOffset: number;
	public byteLength: number;
	public BYTES_PER_ELEMENT: number;
	protected signed: boolean;
	[index: number]: number;

	constructor(
		elementSize: number,
		signed: boolean,
		source: TypedArraySource,
		byteOffset = 0,
		length ? : number,
	) {
		this.BYTES_PER_ELEMENT = elementSize;
		this.signed = signed;
		this.byteOffset = byteOffset;
		const [bufferRead, bufferWrite] =
		bufferBySize[this.BYTES_PER_ELEMENT][this.signed ? 1 : 0];

		// number -> allocate
		if (typeIs(source, "number")) {
			length = source;
			this.byteLength = length * this.BYTES_PER_ELEMENT;
			this.buffer = buffer.create(this.byteLength);
		}

		// existing buffer
		else if (typeIs(source, "buffer")) {
			const totalLen = buffer.len(source);

			this.buffer = source;

			// user provides explicit element length
			if (length !== undefined) {
				// convert to bytes
				this.byteLength = length * this.BYTES_PER_ELEMENT;
			} else {
				// default: use remainder of buffer from byteOffset
				this.byteLength = totalLen - byteOffset;
				length = this.byteLength / this.BYTES_PER_ELEMENT;
			}

			// validate bounds
			if (byteOffset < 0 || byteOffset > totalLen) {
				throw `byteOffset out of bounds`;
			}
			if (byteOffset + this.byteLength > totalLen) {
				throw `byteLength out of bounds`;
			}

			// store final byteOffset and computed length
			this.byteOffset = byteOffset;
		} else if (typeIs(source, "table")) {
			// other typed array
			if ("buffer" in source && "byteLength" in source) {
				// source instanceof TypedArray
				this.byteLength = source.byteLength; // math.floor(buffer.len(source.buffer) - off);
				length = math.floor(
					this.byteLength / this.BYTES_PER_ELEMENT,
				);
				this.buffer = source.buffer;
			}

			// iterable
			else {
				length = source.size();
				// for (const v of source) arr.push(v);
				this.byteLength = length * this.BYTES_PER_ELEMENT;
				this.buffer = buffer.create(this.byteLength);

				for (const i of $range(0, length - 1)) {
					bufferWrite(
						this.buffer,
						i * this.BYTES_PER_ELEMENT,
						source[i],
					);
				}
			}
		}

		// default
		else {
			this.buffer = buffer.create(0);
			this.byteOffset = 0;
			this.byteLength = 0;
			length = 0;
		}

		setmetatable(this, {
			__index: (t: typeof this, i: unknown) => {
				const index = tonumber(i);
				if (index === undefined)
					// typeIs(index, "nil")
					return rawget(t, i) ?? rawget(TypedArray, i);
				// const off = t.byteOffset ?? 0 + idx * elementSize;
				const [_, result] = xpcall(() => bufferRead(
					t.buffer,
					t.byteOffset + index * t.BYTES_PER_ELEMENT,
				), () => 0);

				return result;
			},
			__newindex: (t: typeof this, i: unknown, v: unknown) => {
				const index = tonumber(i);
				if (index === undefined) return rawset(t, i, v);
				const value = tonumber(v) ?? 0;
				// const off = t.byteOffset ?? 0 + idx * elementSize;
				bufferWrite(
					t.buffer,
					t.byteOffset + index * t.BYTES_PER_ELEMENT,
					value,
				);
			},
			__len: () => length,
		});
	}

	subarray(startPos = 0, endPos = getn(this)): this {
		const length = getn(this);
		if (startPos < 0) startPos = math.max(length + startPos, 0);
		if (endPos < 0) endPos = math.max(length + endPos, 0);

		startPos = math.min(startPos, length);
		endPos = math.min(endPos, length);

		const newOffset =
			this.byteOffset + startPos * this.BYTES_PER_ELEMENT;
		const newLength = endPos - startPos;

		// create a new TypedArray view on the same buffer
		return new TypedArray(
			this.BYTES_PER_ELEMENT,
			this.signed,
			this.buffer,
			newOffset,
			newLength,
		) as this;
	}

	set(source: TypedArray | Array < number > , offset = 0): void {
		const bufferWrite =
			bufferBySize[this.BYTES_PER_ELEMENT][this.signed ? 1 : 0][1];
		const thisLength = getn(this);
		const sourceLength = getn(source);
		if (offset < 0 || offset > thisLength)
			throw "offset out of bounds";

		if (offset + sourceLength > thisLength)
			throw "source does not fit";

		if (typeIs(source, "table")) {
			if (
				"buffer" in source &&
				"byteOffset" in source &&
				"byteLength" in source
			) {
				const srcStart = source.byteOffset;
				const dstStart =
					this.byteOffset + offset * this.BYTES_PER_ELEMENT;
				buffer.copy(
					this.buffer,
					dstStart,
					source.buffer,
					srcStart,
					source.byteLength,
				);
			} else {
				for (const i of $range(0, sourceLength - 1)) {
					const value = source[i]; // + 1 - automatically compiles it to be like this
					bufferWrite(
						this.buffer,
						this.byteOffset +
						(offset + i) * this.BYTES_PER_ELEMENT,
						value,
					);
				}
			}
		}
	}

	slice(startPos = 0, endPos = getn(this)): this {
		const view = this.subarray(startPos, endPos);
		const out = new TypedArray(
			view.BYTES_PER_ELEMENT,
			view.signed,
			getn(view),
		) as this;
		out.set(view);
		return out;
	}

	fill(value: number, startPos = 0, endPos = getn(this)): this {
		const len = getn(this);

		if (startPos < 0) startPos = math.max(len + startPos, 0);
		if (endPos < 0) endPos = math.max(len + endPos, 0);

		startPos = math.clamp(startPos, 0, len);
		endPos = math.clamp(endPos, 0, len);

		for (const i of $range(startPos, endPos - 1)) {
			this[i] = value;
		}

		return this;
	}

	// ===== copyWithin(target, startPos, endPos?) =====
	copyWithin(
		target: number,
		startPos: number,
		endPos = getn(this),
	): this {
		const len = getn(this);

		// normalize negative numbers
		if (target < 0) target = len + target;
		if (startPos < 0) startPos = len + startPos;
		if (endPos < 0) endPos = len + endPos;

		target = math.clamp(target, 0, len);
		startPos = math.clamp(startPos, 0, len);
		endPos = math.clamp(endPos, 0, len);

		const count = math.min(endPos - startPos, len - target);
		if (count <= 0) return this;

		const elementSize = this.BYTES_PER_ELEMENT;

		const srcOffset = this.byteOffset + startPos * elementSize;
		const dstOffset = this.byteOffset + target * elementSize;
		const byteLength = count * elementSize;

		// use buffer.copy, works even for overlapping regions
		buffer.copy(
			this.buffer,
			dstOffset,
			this.buffer,
			srcOffset,
			byteLength,
		);

		/*
		// temp buffer to preserve overlap semantics
		for (const i of $range(0, count - 1)) {
			this[target + i] = this[startPos + i];
		}
		*/

		return this;
	}
}

// ---- typed arrays ----
class Uint8Array extends TypedArray {
	constructor(
		source: TypedArraySource,
		byteOffset ? : number,
		length ? : number,
	) {
		super(1, false, source, byteOffset, length);
	}
}

class Uint16Array extends TypedArray {
	constructor(
		source: TypedArraySource,
		byteOffset ? : number,
		length ? : number,
	) {
		super(2, false, source, byteOffset, length);
	}
}

class Int16Array extends TypedArray {
	constructor(
		source: TypedArraySource,
		byteOffset ? : number,
		length ? : number,
	) {
		super(2, true, source, byteOffset, length);
	}
}

class Int32Array extends TypedArray {
	constructor(
		source: TypedArraySource,
		byteOffset ? : number,
		length ? : number,
	) {
		super(4, true, source, byteOffset, length);
	}
}


// aliases for shorter compressed code (most minifers don't do this)
const u8 = Uint8Array,
	u16 = Uint16Array,
	i16 = Int16Array,
	i32 = Int32Array;

// Huffman decoding table
interface HDT {
	// initial bits
	b: number;
	// symbols
	s: Uint8Array;
	// num bits
	n: Uint8Array;
}

// FSE decoding table
interface FSEDT extends HDT {
	// next state
	t: Uint16Array;
}

// decompress Zstandard state
interface DZstdState {
	// byte
	b: number;
	// out byte
	y: number;
	// dictionary ID
	d: number;
	// window
	w: Uint8Array;
	// max block size
	m: number;
	// uncompressed size
	u: number;
	// has checksum
	c: number;
	// offsets
	o: Int32Array;
	// window head
	e: number;
	// last huffman decoding table
	h ? : HDT;
	// last FSE decoding tables
	t ? : [FSEDT, FSEDT, FSEDT];
	// last block
	l: number;
}

const rb = (d: Uint8Array, b: number, n: number) => { // buffer readbits
	let o = 0;
	for (const i of $range(0, n - 1)) o |= d[b++] << (i << 3);
	//   for (; i < n; ++i) o |= d[b++] << (i << 3);
	return o;
};

// apply a dictionary to decompressor state
const rdic = (dic: Uint8Array, st: DZstdState) => {
	if (rb(dic, 0, 4) === 0xec30a437) {
		if (st.d && st.d !== rb(dic, 4, 4)) throw 'wrong dictionary';
		let bt = 8;
		[bt, st.h] = rhu(dic, bt);
		let mlt: FSEDT, olt: FSEDT, llt: FSEDT;
		[bt, olt] = rfse(dic, bt, 8);
		[bt, mlt] = rfse(dic, bt, 9);
		[bt, llt] = rfse(dic, bt, 9);
		st.t = [mlt, olt, llt];
		st.o = new i32([rb(dic, bt, 4), rb(dic, bt + 4, 4), rb(dic, bt + 8, 4)]);
		dic = dic.subarray(bt + 12);
	}
	st.w = dic.slice();
	st.e = getn(dic);
};

// read Zstandard frame header
const rzfh = (dat: Uint8Array, w ? : Uint8Array | 1): number | DZstdState => {
	const n3 = rb(dat, 0, 3); // buffer.readbits(dat.buffer, 0, 24); // dat[0] | (dat[1] << 8) | (dat[2] << 16);
	if (tonumber(n3) === 0x2FB528 && tonumber(dat[3]) === 253) {
		// Zstandard
		const flg = dat[4];
		//    single segment       checksum             dict flag     frame content flag
		const ss = ((flg >> 5) & 1),
			cc = ((flg >> 2) & 1),
			df = (flg & 3),
			fcf = (flg >> 6);
		if (flg & 8) throw 'invalid zstd data';
		// byte
		let bt = 6 - ss;
		// dict bytes
		const db = tonumber(df) === 3 ? 4 : df;
		// dictionary id
		const di = rb(dat, bt, db);
		bt += db;
		// frame size bytes
		const fsb = fcf ? (1 << fcf) : ss;
		// frame source size
		const fss = rb(dat, bt, fsb) + ((tonumber(fcf) === 1) ? 256 : 0);
		// window size
		let ws = fss;
		if (!ss) {
			// window descriptor
			const wb = (1 << (10 + (dat[5] >> 3)));
			ws = ((wb) + ((wb >> 3) * (dat[5] & 7)));
		}
		if (ws > 2145386496) throw 'window size too large (>2046MB)';
		const buf = new u8((tonumber(w) === 1 ? (fss || ws) : w ? 0 : ws) + 12);
		buf[0] = 1, buf[4] = 4, buf[8] = 8;
		return {
			b: bt + fsb,
			y: 0,
			l: 0,
			d: di,
			w: ((w && tonumber(w) !== 1) ? w : buf.subarray(12)) as Uint8Array,
			e: ws,
			o: new i32(buf.buffer, 0, 3),
			u: fss,
			c: cc,
			m: math.min(131072, ws)
		};
	} else if (tonumber(((n3 >> 4) | (dat[3] << 20))) === 0x184D2A5) {
		// skippable
		return rb(dat, 4, 4) + 8; // buffer.readu32(dat.buffer, 4) + 8; // b4(dat, 4) + 8;
	}
	throw 'invalid zstd data';
};

// most significant bit for nonzero
const msb = (val: number) => {
	let bits = 0;
	while (((1 << bits)) <= val) bits++
	return bits - 1;
};

// read finite state entropy
const rfse = (dat: Uint8Array, bt: number, mal: number): [number, FSEDT] => {
	// table pos
	let tpos = (bt << 3) + 4;
	// accuracy log
	const al = ((dat[bt] & 15)) + 5;
	if (al > mal) throw 'FSE accuracy too high';
	// size
	const sz = (1 << al);
	// probabilities symbols  repeat   index   high threshold
	let probs = sz,
		sym = -1,
		re = -1,
		i = -1,
		ht = sz;
	// optimization: single allocation is much faster
	const buf = buffer.create(512 + ((sz) << 2));
	const freq = new i16(buf, 0, 256);
	// same view as freq
	const dstate = new u16(buf, 0, 256);
	const nstate = new u16(buf, 512, sz);
	const bb1 = 512 + ((sz) << 1);
	const syms = new u8(buf, bb1, sz);
	const nbits = new u8(buf, bb1 + sz);
	while (sym < 255 && probs > 0) {
		const bits = msb((probs + 1));
		const cbt = (tpos) >> 3;
		// mask
		const msk = ((1 << ((bits + 1))) - 1);
		let val = ((rb(dat, cbt, 3) >> (tpos & 7)) & msk);
		// mask (1 fewer bit)
		const msk1fb = ((1 << bits) - 1);
		// max small value
		const msv = ((msk) - (probs) - 1);
		// small value
		const sval = (val & msk1fb);
		if (sval < msv) tpos += bits, val = sval;
		else {
			tpos += (bits + 1);
			if ((val) > (msk1fb)) val = (val) - (msv);
		}
		freq[++sym] = --val;
		if (tonumber(val) === -1) {
			probs += val;
			syms[--ht] = sym;
		} else probs -= val;
		if (!val) {
			do {
				// repeat byte
				const rbt = (tpos) >> 3;
				re = ((rb(dat, rbt, 2) >> (tpos & 7)) & 3);
				tpos += 2;
				sym += re;
			} while (tonumber(re) === 3);
		}
	}
	if (sym > 255 || probs) throw 'invalid zstd data';
	let sympos = 0;
	// sym step (coprime with sz - formula from zstd source)
	const sstep = ((sz >> 1) + (sz >> 3) + 3);
	// sym mask
	const smask = (sz - 1);
	for (const s of $range(0, sym)) { // let s = 0; s <= sym; ++s
		const sf = freq[s];
		if (sf < 1) {
			dstate[s] = -sf;
			continue;
		}
		// This is split into two loops in zstd to avoid branching, but as JS is higher-level that is unnecessary
		for (const n of $range(0, sf - 1)) {
			i = n;
			syms[sympos] = s;
			do {
				sympos = ((sympos) + (sstep)) & smask
			} while (sympos >= ht)
		}

	}
	// After spreading symbols, should be zero again
	if (sympos) throw 'invalid zstd data';
	for (const n of $range(0, sz - 1)) {
		i = n // assign to the external i
		// next state
		const ns = dstate[syms[i]]++
		// num bits
		const nb = nbits[i] = (al - msb(ns))
		nstate[i] = ((ns << nb) - sz)
	}

	return [((tpos + 7) >> 3), {
		b: al,
		s: syms,
		n: nbits,
		t: nstate
	}];
};

// read huffman
const rhu = (dat: Uint8Array, bt: number): [number, HDT] => {
	//  index  weight count
	let i = 0,
		wc = -1;
	//    buffer             header byte
	const buf = new u8(292),
		hb = dat[bt];
	// huffman weights
	const hw = buf.subarray(0, 256);
	// rank count
	const rc = buf.subarray(256, 268);
	// rank index
	const ri = new u16(buf.buffer, 268);
	// NOTE: at this point bt is 1 less than expected
	if ((hb) < 128) {
		// end byte, fse decode table
		const [ebt, fdt] = rfse(dat, (bt + 1), 6);
		bt += hb;
		const epos = (ebt << 3);
		// last byte
		const lb = dat[bt];
		if (!lb) throw 'invalid zstd data';
		//  state1   state2   state1 bits   state2 bits
		let st1 = 0,
			st2 = 0,
			btr1 = fdt.b,
			btr2 = btr1;
		// fse pos
		// pre-increment to account for original deficit of 1
		let fpos = ((++bt << 3) - 8 + msb(lb));
		while (true) { // for(;;)
			fpos -= btr1;
			if (fpos < epos) break;
			let cbt = (fpos) >> 3;
			st1 += ((rb(dat, cbt, 2) >> (fpos & 7)) & (((1 << btr1) - 1)));
			hw[++wc] = fdt.s[st1];
			fpos -= btr2;
			if (fpos < epos) break;
			cbt = (fpos) >> 3;
			st2 += ((rb(dat, cbt, 2) >> (fpos & 7)) & (((1 << btr2) - 1)));
			hw[++wc] = fdt.s[st2];
			btr1 = fdt.n[st1];
			st1 = fdt.t[st1];
			btr2 = fdt.n[st2];
			st2 = fdt.t[st2];
		}
		if ((++wc) > 255) throw 'invalid zstd data';
	} else {
		wc = (hb - 127);
		while (i < wc) {
			const byte = dat[++bt];
			hw[i] = (byte >> 4);
			hw[i + 1] = (byte & 15);
			i += 2;
		}


		++bt;
	}
	// weight exponential sum
	let wes = 0;
	for (const n of $range(0, wc - 1)) {
		i = n;
		const wt = hw[i];
		// bits must be at most 11, same as weight
		if (wt > 11) throw 'invalid zstd data';
		wes += wt && ((1 << ((wt - 1))));
	}

	// max bits
	const mb = msb(wes) + 1;
	// table size
	const ts = (1 << mb);
	// remaining sum
	const rem = (ts - wes);
	// must be power of 2
	if ((rem) & (rem - 1)) throw 'invalid zstd data';
	hw[wc++] = (msb(rem) + 1);
	for (const n of $range(0, wc - 1)) {
		i = n;
		const wt = hw[i];
		++rc[(hw[i] = wt && ((mb + 1 - wt)))];
	}

	// huf buf
	const hbuf = new u8((ts << 1));
	//    symbols                      num bits
	const syms = hbuf.subarray(0, ts),
		nb = hbuf.subarray(ts);
	ri[mb] = 0;
	for (const n of $range(mb, 1, -1)) {
		i = n;
		const pv = ri[i];
		nb.fill(i, pv, ri[i - 1] = ((pv) + (rc[i] * ((1 << ((mb - i)))))));
	}

	if (tonumber(ri[0]) !== tonumber(ts)) throw 'invalid zstd data';
	for (const n of $range(0, wc - 1)) {
		i = n;
		const bits = hw[i];
		if (bits) {
			const code = ri[bits];
			syms.fill(i, code, ri[bits] = ((code) + (1 << ((mb - bits)))));
		}
	}
	/*
	for (i = 0; i < wc; ++i) {
	  const bits = hw[i];
	  if (bits) {
	    const code = ri[bits];
	    syms.fill(i, code, ri[bits] = code + (1 << (mb - bits)));
	  }
	}
	*/
	return [bt, {
		n: nb,
		b: mb,
		s: syms
	}];
};

// Tables generated using this:
// https://gist.github.com/101arrowz/a979452d4355992cbf8f257cbffc9edd

// default literal length table
const dllt = /*#__PURE__*/ rfse( /*#__PURE__*/ new u8([
	81, 16, 99, 140, 49, 198, 24, 99, 12, 33, 196, 24, 99, 102, 102, 134, 70, 146, 4
]), 0, 6)[1];

// default match length table
const dmlt = /*#__PURE__*/ rfse( /*#__PURE__*/ new u8([
	33, 20, 196, 24, 99, 140, 33, 132, 16, 66, 8, 33, 132, 16, 66, 8, 33, 68, 68, 68, 68, 68, 68, 68, 68, 36, 9
]), 0, 6)[1];

// default offset code table
const doct = /*#__PURE__ */ rfse( /*#__PURE__ */ new u8([
	32, 132, 16, 66, 102, 70, 68, 68, 68, 68, 36, 73, 2
]), 0, 5)[1];

// bits to baseline
const b2bl = (b: Uint8Array, s: number) => {
	const len = getn(b),
		bl = new i32(len);
	for (const i of $range(0, len - 1)) {
		bl[i] = s;
		s += (1 << b[i]);
	}
	return bl;
};

// literal length bits
const llb = /*#__PURE__ */ new u8(( /*#__PURE__ */ new i32([
	0, 0, 0, 0, 16843009, 50528770, 134678020, 202050057, 269422093
])).buffer, 0, 36);

// literal length baseline
const llbl = /*#__PURE__ */ b2bl(llb, 0);

// match length bits
const mlb = /*#__PURE__ */ new u8(( /*#__PURE__ */ new i32([
	0, 0, 0, 0, 0, 0, 0, 0, 16843009, 50528770, 117769220, 185207048, 252579084, 16
])).buffer, 0, 53);

// match length baseline
const mlbl = /*#__PURE__ */ b2bl(mlb, 3);

// decode huffman stream
const dhu = (dat: Uint8Array, out: Uint8Array, hu: HDT) => {
	const len = getn(dat),
		ss = getn(out),
		lb = dat[len - 1],
		msk = ((1 << hu.b) - 1),
		eb = -hu.b;
	if (!lb) throw 'invalid zstd data';
	let st = 0,
		btr = hu.b,
		pos = ((len << 3) - 8 + msb(lb) - btr),
		i = -1;
	while (pos > eb && i < ss) { // for (; pos > eb && i < ss;)
		const cbt = toSigned(bit32.arshift(pos, 3));
		const val = (rb(dat, cbt, 3) >> (pos & 7));
		st = ((st << btr) | val) & msk;
		out[++i] = hu.s[st];
		pos -= (btr = hu.n[st]);
	}
	if (tonumber(pos) !== tonumber(eb) || tonumber(i + 1) !== tonumber(ss)) throw 'invalid zstd data';
};

// decode huffman stream 4x
// TODO: use workers to parallelize
const dhu4 = (dat: Uint8Array, out: Uint8Array, hu: HDT) => {
	let bt = 6;
	const ss = getn(out),
		sz1 = ((ss + 3) >> 2),
		sz2 = (sz1 << 1),
		sz3 = (sz1 + sz2);
	dhu(dat.subarray(bt, bt += rb(dat, 0, 2)), out.subarray(0, sz1), hu);
	dhu(dat.subarray(bt, bt += rb(dat, 2, 2)), out.subarray(sz1, sz2), hu);
	dhu(dat.subarray(bt, bt += rb(dat, 4, 2)), out.subarray(sz2, sz3), hu);
	dhu(dat.subarray(bt), out.subarray(sz3), hu);
};

// read Zstandard block
const rzb = (dat: Uint8Array, st: DZstdState, out ? : Uint8Array) => {
	let bt = st.b;
	//    byte 0        block type
	const b0 = dat[bt],
		btype = ((b0 >> 1) & 3);
	st.l = (b0 & 1);
	const sz = (((b0 >> 3) | (dat[bt + 1] << 5) | (dat[bt + 2] << 13)));
	// end byte for block
	const ebt = (bt += 3) + sz;
	if (tonumber(btype) === 1) {
		if (bt >= getn(dat)) return;
		st.b = bt + 1;
		if (out) {
			out.fill(dat[bt], st.y, st.y += sz);
			return out;
		}
		return new u8(sz).fill(dat[bt]);
	}
	if (ebt > getn(dat)) return;
	if (tonumber(btype) === 0) {
		st.b = ebt;
		if (out) {
			out.set(dat.subarray(bt, ebt), st.y);
			st.y += sz;
			return out;
		}
		return dat.slice(bt, ebt);
	}
	if (tonumber(btype) === 2) {
		//    byte 3        lit btype     size format
		const b3 = dat[bt],
			lbt = (b3 & 3),
			sf = ((b3 >> 2) & 3);
		// lit src size  lit cmp sz 4 streams
		let lss = (b3 >> 4),
			lcs = 0,
			s4 = 0;
		if (lbt < 2) {
			if (sf & 1) lss |= ((dat[++bt] << 4) | ((sf & 2) && (dat[++bt] << 12)));
			else lss = (b3 >> 3);
		} else {
			s4 = sf;
			if (sf < 2) lss |= (((dat[++bt] & 63) << 4)), lcs = ((dat[bt] >> 6) | (dat[++bt] << 2));
			else if (tonumber(sf) === 2) lss |= (dat[++bt] << 4) | ((dat[++bt] & 3) << 12), lcs = ((dat[bt] >> 2) | (dat[++bt] << 6));
			else lss |= (dat[++bt] << 4) | ((dat[++bt] & 63) << 12), lcs = ((dat[bt] >> 6) | (dat[++bt] << 2) | (dat[++bt] << 10));
		}
		++bt;
		// add literals to end - can never overlap with backreferences because unused literals always appended
		let buf = out ? out.subarray(st.y, st.y + st.m) : new u8(st.m);
		// starting point for literals
		let spl = (getn(buf) - lss);
		if (tonumber(lbt) === 0) buf.set(dat.subarray(bt, bt += lss), spl);
		else if (tonumber(lbt) === 1) buf.fill(dat[bt++], spl);
		else {
			// huffman table
			let hu = st.h;
			if (tonumber(lbt) === 2) {
				const hud = rhu(dat, bt);
				// subtract description length
				lcs += (bt - (bt = hud[0]));
				st.h = hu = hud[1];
			} else if (!hu) throw 'invalid zstd data';
			(s4 ? dhu4 : dhu)(dat.subarray(bt, bt += lcs), buf.subarray(spl), hu);
		}
		// num sequences
		let ns = dat[bt++];
		if (ns) {
			if (tonumber(ns) === 255) ns = ((dat[bt++] | (dat[bt++] << 8)) + 0x7F00);
			else if (ns > 127) ns = (((ns - 128) << 8) | dat[bt++]);
			// symbol compression modes
			const scm = dat[bt++];
			if ((scm) & 3) throw 'invalid zstd data';
			const dts: [FSEDT, FSEDT, FSEDT] = [dmlt, doct, dllt];
			for (const i of $range(2, 0, -1)) {
				const md = ((scm >> ((i << 1) + 2)) & 3);
				if (tonumber(md) === 1) {
					// rle buf
					const rbuf = new u8([0, 0, dat[bt++]]);
					dts[i] = {
						s: rbuf.subarray(2, 3),
						n: rbuf.subarray(0, 1),
						t: new u16(rbuf.buffer, 0, 1),
						b: 0
					};
				} else if (tonumber(md) === 2) {
					// accuracy log 8 for offsets, 9 for others
					[bt, dts[i]] = rfse(dat, bt, (9 - (i & 1)));
				} else if (tonumber(md) === 3) {
					if (!st.t) throw 'invalid zstd data';
					dts[i] = st.t[i];
				}
			}
			const [mlt, oct, llt] = st.t = dts;
			const lb = dat[ebt - 1];
			if (!lb) throw 'invalid zstd data';
			let spos = ((ebt << 3) - 8 + msb(lb) - llt.b),
				cbt = (spos >> 3),
				oubt = 0;
			let lst = ((rb(dat, cbt, 2) >> (spos & 7)) & (((1 << llt.b) - 1)));
			cbt = ((spos -= oct.b) >> 3);
			let ost = ((rb(dat, cbt, 2) >> (spos & 7)) & (((1 << oct.b) - 1)));
			cbt = ((spos -= mlt.b) >> 3);
			let mst = ((rb(dat, cbt, 2) >> (spos & 7)) & (((1 << mlt.b) - 1)));
			ns++;
			while (--ns) {
				const llc = llt.s[lst];
				const lbtr = llt.n[lst];
				const mlc = mlt.s[mst];
				const mbtr = mlt.n[mst];
				const ofc = oct.s[ost];
				const obtr = oct.n[ost];

				cbt = ((spos -= ofc) >> 3);
				const ofp = (1 << ofc);
				let off = (ofp + ((rb(dat, cbt, 4) >>> (spos & 7)) & (ofp - 1)));
				cbt = ((spos -= mlb[mlc]) >> 3);
				let ml = (mlbl[mlc] + ((rb(dat, cbt, 3) >> (spos & 7)) & (((1 << mlb[mlc]) - 1))));
				cbt = ((spos -= llb[llc]) >> 3);
				const ll = (llbl[llc] + ((rb(dat, cbt, 3) >> (spos & 7)) & (((1 << llb[llc]) - 1))));

				cbt = ((spos -= lbtr) >> 3);
				lst = (llt.t[lst] + ((rb(dat, cbt, 2) >> (spos & 7)) & (((1 << lbtr) - 1))));
				cbt = ((spos -= mbtr) >> 3);
				mst = (mlt.t[mst] + ((rb(dat, cbt, 2) >> (spos & 7)) & (((1 << mbtr) - 1))));
				cbt = ((spos -= obtr) >> 3);
				ost = (oct.t[ost] + ((rb(dat, cbt, 2) >> (spos & 7)) & (((1 << obtr) - 1))));

				if ((off) > 3) {
					st.o[2] = st.o[1];
					st.o[1] = st.o[0];
					st.o[0] = (off -= 3);
				} else {
					const idx = (off - ((ll) !== 0 ? 1 : 0));
					if ((idx)) {
						off = (tonumber(idx) === 3 ? st.o[0] - 1 : st.o[idx]);
						if ((idx) > 1) st.o[2] = st.o[1];
						st.o[1] = st.o[0];
						st.o[0] = off;
					} else off = st.o[0];
				}
				for (const i of $range(0, ll - 1)) {
					buf[oubt + i] = buf[spl + i];
				}

				oubt += ll, spl += ll;
				let stin = (oubt - off);
				if (stin < 0) {
					let len = -stin;
					const bs = (st.e + stin);
					if ((len) > ml) len = ml;
					for (const i of $range(0, len - 1)) {
						buf[oubt + i] = st.w[bs + i];
					}
					oubt += len, ml -= len, stin = 0;
				}
				for (const i of $range(0, ml - 1)) {
					buf[oubt + i] = buf[stin + i];
				}
				oubt += ml;
			}
			if ((oubt) !== (spl)) {
				while (spl < getn(buf)) {
					buf[oubt++] = buf[spl++];
				}
			} else oubt = getn(buf);
			if (out) st.y += oubt;
			else buf = buf.slice(0, oubt);
		} else if (out) {
			st.y += lss;
			if (spl) {
				for (const i of $range(0, lss - 1)) {
					buf[i] = buf[spl + i];
				}
			}
		} else if (spl) buf = buf.slice(spl);
		st.b = ebt;
		return buf;
	}
	throw 'invalid block type';
};

// concat
const cct = (bufs: Uint8Array[], ol: number) => {
	if (tonumber(getn(bufs)) === 1) return bufs[0];
	const buf = new u8(ol);
	let b = 0
	for (const i of $range(0, getn(bufs) - 1)) {
		const chk = bufs[i]
		buf.set(chk, b)
		b += getn(chk)
	}

	return buf;
};

/**
 * Decompresses Zstandard data
 * @param data The input data
 * @param decompressedSize The decompressed size. If unspecified, the function will allocate
 *            exactly enough memory to fit the decompressed data. If your
 *            data has multiple frames and you know the output size, specifying
 *            it will yield better performance.
 * @returns The decompressed data
 */
export function decompress(data: buffer, decompressedSize ? : number, dictionary ? : buffer) {
	if (!typeIs(data, "buffer")) throw "expected buffer";
	const dic = typeIs(dictionary, "buffer") ? new u8(dictionary) : undefined;
	let buf = typeIs(decompressedSize, "number") ? new u8(decompressedSize) : undefined;
	let dat = new u8(data);
	const bufs: Uint8Array[] = [],
		nb = !buf ? 1 : 0 as 0 | 1;
	let bt = 0,
		ol = 0;
	while (getn(dat)) {
		const st = rzfh(dat, nb || buf);
		if (typeIs(st, 'table')) {
			if (st.d && !dic) throw 'wrong dictionary';
			if (dic) rdic(dic, st);
			if (nb) {
				buf = undefined;
				if (tonumber(getn(st.w)) === tonumber(st.u)) {
					bufs.push(buf = st.w);
					ol += st.u;
				}
			} else {
				bufs.push(buf as Uint8Array);
				st.e = 0;
			}
			while (!st.l) {
				const blk = rzb(dat, st, buf);
				if (!blk) throw 'unexpected EOF';
				if (buf) st.e = st.y;
				else {
					bufs.push(blk);
					ol += getn(blk);
					st.w.copyWithin(0, getn(blk));
					st.w.set(blk, getn(st.w) - getn(blk));
				}
			}
			bt = (st.b + (st.c * 4));
		} else bt = st as number;
		dat = dat.subarray(bt);
	}

	return toBuffer(cct(bufs, ol));
}

/**
 * Callback to handle data in Zstandard streams
 * @param data The data that was (de)compressed
 * @param final Whether this is the last chunk in the stream
 */
export type ZstdStreamHandler = (data: buffer, final ? : boolean) => unknown;

/**
 * Decompressor for Zstandard streamed data
 */
export class Decompress {
	private s!: DZstdState | number;
	private c: Uint8Array[];
	private l: number;
	private z: number;
	private d!: Uint8Array;
	/**
	 * Creates a Zstandard decompressor
	 * @param ondata The handler for stream data
	 */
	constructor(ondata: ZstdStreamHandler) {
		this.ondata = ondata;
		this.c = [];
		this.l = 0;
		this.z = 0;
	}

	/**
	 * Loads a dictionary
	 * @param dic The dictionary buffer. May be a Zstandard-formatted dictionary
	 *            (as generated by zstd --train) or raw content.
	 */
	loadDictionary(dic: buffer) {
		if (!typeIs(dic, "buffer")) throw "expected buffer";
		this.d = new u8(dic);
	}

	/**
	 * Pushes data to be decompressed
	 * @param chunk The chunk of data to push
	 * @param final Whether or not this is the last chunk in the stream
	 */
	push(buf: buffer, final ? : boolean): undefined {
		if (!typeIs(buf, "buffer")) throw "expected buffer";
		let chunk = new u8(buf);
		if (typeIs(this.s, 'number')) {
			const sub = math.min(getn(chunk), this.s as number);
			chunk = chunk.subarray(sub);
			(this.s as number) -= sub;
		}
		const sl = getn(chunk);
		const ncs = sl + this.l;
		if (!this.s) {
			if (final) {
				if (!ncs) {
					this.ondata(toBuffer(new u8(0)), true);
					return;
				}
				// min for frame + one block
				if (ncs < 5) throw 'unexpected EOF';
			} else if (ncs < 18) {
				this.c.push(chunk);
				this.l = ncs;
				return;
			}
			if (this.l) {
				this.c.push(chunk);
				chunk = cct(this.c, ncs);
				this.c = [];
				this.l = 0;
			}
			if (typeIs((this.s = rzfh(chunk)), 'number')) return this.push(toBuffer(chunk), final);
			if (this.s.d && !this.d) throw 'wrong dictionary';
			if (this.d) rdic(this.d, this.s);
		}
		if (!typeIs(this.s, 'number')) {
			if (ncs < (this.z || 3)) {
				if (final) throw 'unexpected EOF';
				this.c.push(chunk);
				this.l = ncs;
				return;
			}
			if (this.l) {
				this.c.push(chunk);
				chunk = cct(this.c, ncs);
				this.c = [];
				this.l = 0;
			}
			if (!this.z && ncs < (this.z = (chunk[(this.s as DZstdState).b] & 2) ? 4 : 3 + ((chunk[(this.s as DZstdState).b] >> 3) | (chunk[(this.s as DZstdState).b + 1] << 5) | (chunk[(this.s as DZstdState).b + 2] << 13)))) {
				if (final) throw 'unexpected EOF';
				this.c.push(chunk);
				this.l = ncs;
				return;
			} else this.z = 0;
			while (true) {
				const blk = rzb(chunk, this.s as DZstdState);
				if (!blk) {
					if (final) throw 'unexpected EOF';
					const adc = chunk.subarray((this.s as DZstdState).b);
					(this.s as DZstdState).b = 0;
					this.c.push(adc), this.l += getn(adc);
					return;
				} else {
					const blk1 = new Uint8Array(toBuffer(blk));  // create a full copy

					this.ondata(blk1.buffer, false);
					(this.s as DZstdState).w.copyWithin(0, getn(blk1));
					(this.s as DZstdState).w.set(blk1, getn((this.s as DZstdState).w) - getn(blk1));
				}
				if ((this.s as DZstdState).l) {
					const rest = chunk.subarray((this.s as DZstdState).b);
					this.s = (this.s as DZstdState).c * 4;
					this.push(toBuffer(rest), final);
					return;
				}
			}
		} else if (final) throw 'unexpected EOF';
	}

	/**
	 * Handler called whenever data is decompressed
	 */
	ondata: ZstdStreamHandler;
}

/* example
const dictionaryBytes = [
	0x37, 0xa4, 0x30, 0xec, 0x33, 0x4b, 0xd1, 0x42, 0x2a, 0x10, 0xc8, 0x92,
	0x24, 0x1d, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff, 0xbf, 0x6c, 0x26, 0x52,
	0x6e, 0xd9, 0xc8, 0x96, 0xb7, 0x41, 0xd2, 0x36, 0x2b, 0xf7, 0x86, 0xf9,
	0x7f, 0x1a, 0xa9, 0x95, 0x45, 0xa1, 0xcf, 0xc6, 0xf3, 0xed, 0xe5, 0x7a,
	0x55, 0xc6, 0x01, 0x03, 0x00, 0x00, 0x60, 0x90, 0xe0, 0x40, 0x32, 0xcb,
	0xde, 0x02, 0x00, 0x00, 0x24, 0xa0, 0xd0, 0x8e, 0x0c, 0x8b, 0x09, 0x08,
	0x87, 0x69, 0x10, 0x03, 0x19, 0x42, 0x0c, 0x21, 0x84, 0x10, 0x43, 0x08,
	0x21, 0x84, 0x10, 0x42, 0x08, 0x21, 0x64, 0xd2, 0x88, 0x88, 0x24, 0x49,
	0x92, 0x0e, 0x04, 0x69, 0x64, 0xc2, 0xc5, 0xc3, 0xa2, 0x22, 0xd1, 0x54,
	0x10, 0x10, 0xc7, 0x82, 0x2c, 0x12, 0x52, 0x50, 0x4a, 0xa7, 0x6c, 0x24,
	0x49, 0x92, 0x0e, 0x01, 0x00, 0x00, 0x00, 0x04, 0x00, 0x00, 0x00, 0x08,
	0x00, 0x00, 0x00, 0x73, 0x20, 0x61, 0x75, 0x63, 0x74, 0x6f, 0x72, 0x20,
	0x65, 0x20, 0x65, 0x74, 0x20, 0x65, 0x66, 0x66, 0x69, 0x63, 0x69, 0x74,
	0x75, 0x72, 0x20, 0x64, 0x75, 0x69, 0x20, 0x65, 0x6c, 0x65, 0x6d, 0x65,
	0x6e, 0x74, 0x75, 0x6d, 0x20, 0x61, 0x74, 0x2e, 0x20, 0x50, 0x68, 0x61,
	0x73, 0x65, 0x6c, 0x6c, 0x75, 0x73, 0x20, 0x61, 0x6c, 0x69, 0x71, 0x75,
	0x65, 0x74, 0x20, 0x65, 0x73, 0x75, 0x61, 0x64, 0x61, 0x20, 0x66, 0x61,
	0x6d, 0x65, 0x73, 0x20, 0x61, 0x63, 0x20, 0x74, 0x75, 0x72, 0x70, 0x69,
	0x73, 0x20, 0x65, 0x67, 0x65, 0x73, 0x74, 0x61, 0x73, 0x2e, 0x20, 0x41,
	0x6c, 0x69, 0x71, 0x75, 0x61, 0x6d, 0x20, 0x66, 0x61, 0x63, 0x69, 0x6c,
	0x69, 0x73, 0x69, 0x73, 0x20, 0x6c, 0x20, 0x6c, 0x6f, 0x62, 0x6f, 0x72,
	0x74, 0x69, 0x73, 0x20, 0x69, 0x70, 0x73, 0x75, 0x6d, 0x2e, 0x20, 0x53,
	0x75, 0x73, 0x70, 0x65, 0x6e, 0x64, 0x69, 0x73, 0x73, 0x65, 0x20, 0x70,
	0x6f, 0x72, 0x74, 0x74, 0x69, 0x74, 0x6f, 0x72, 0x2c, 0x20, 0x6c, 0x69,
	0x67, 0x75, 0x6c, 0x61, 0x20, 0x61, 0x20, 0x61, 0x63, 0x75, 0x6c, 0x69,
	0x73, 0x20, 0x6c, 0x61, 0x63, 0x75, 0x73, 0x20, 0x75, 0x74, 0x2c, 0x20,
	0x62, 0x6c, 0x61, 0x6e, 0x64, 0x69, 0x74, 0x20, 0x70, 0x75, 0x72, 0x75,
	0x73, 0x2e, 0x20, 0x4d, 0x61, 0x65, 0x63, 0x65, 0x6e, 0x61, 0x73, 0x20,
	0x73, 0x69, 0x74, 0x20, 0x61, 0x6d, 0x65, 0x74, 0x20, 0x6e, 0x74, 0x20,
	0x6d, 0x6f, 0x72, 0x62, 0x69, 0x20, 0x74, 0x72, 0x69, 0x73, 0x74, 0x69,
];
const data = [
	0x28, 0xb5, 0x2f, 0xfd, 0x03, 0x68, 0x33, 0x4b, 0xd1, 0x42, 0x65, 0x02,
	0x00, 0xa3, 0x05, 0x0d, 0x07, 0xbb, 0x82, 0xfa, 0x96, 0xdb, 0x4a, 0x5d,
	0x73, 0xbf, 0xd6, 0x6e, 0x7d, 0x69, 0xa9, 0x14, 0x5b, 0x3b, 0x7e, 0x4e,
	0xb7, 0xbf, 0xb0, 0x1d, 0xa1, 0x7e, 0x9d, 0x52, 0xcd, 0xe3, 0x87, 0x53,
	0xe1, 0x29, 0x9a, 0xa8, 0x25, 0x93, 0xe9, 0x90, 0x11, 0x36, 0x65, 0xe8,
	0xcb, 0x32, 0xc5, 0xd2, 0x17, 0x2e, 0x13, 0x42, 0x08, 0xfc, 0x1e, 0x33,
	0xa8, 0xac, 0xa2, 0x97, 0x18, 0xfe, 0x6e, 0xde, 0xf4, 0x34, 0xd8, 0xbc,
	0x89, 0x4f, 0x51, 0xe7, 0x0b,
];
const compressed = new Uint8Array(data).buffer;
const dictionary = new Uint8Array(dictionaryBytes).buffer;
const expected =
	"Lorem ipsum dolor sit amet, consectetur adipiscing elit. Nullam nec sem urna. Morbi mollis, massa a convallis iaculis, mauris neque.";
{
	const decompressed = decompress(compressed, undefined, dictionary);
	const shouldBeLoremIpsum = buffer.tostring(decompressed);
	assert(expected === shouldBeLoremIpsum);
} {
	let outputChunks: Uint8Array[] = [];
	const decompressor = new Decompress(data => outputChunks.push(data));
	decompressor.loadDictionary(dictionary);
	decompressor.push(compressed, true);
	let output = new Uint8Array(
		outputChunks.reduce((acc, cur) => acc + cur.byteLength, 0),
	);
	for (let i = 0, offset = 0; i < getn(outputChunks); i++) {
		output.set(outputChunks[i], offset);
		offset += getn(outputChunks[i]);
	}
	let shouldBeLoremIpsum = buffer.tostring(toBuffer(output));
	assert(expected === shouldBeLoremIpsum);
}
*/
