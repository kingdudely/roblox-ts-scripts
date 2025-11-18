-- Compiled with roblox-ts v3.0.0
-- Some numerical data is initialized as -1 even when it doesn't need initialization to help the JIT infer types
local bufferBySize = {
	[1] = { { buffer.readu8, buffer.writeu8 }, { buffer.readi8, buffer.writei8 } },
	[2] = { { buffer.readu16, buffer.writeu16 }, { buffer.readi16, buffer.writei16 } },
	[4] = { { buffer.readu32, buffer.writeu32 }, { buffer.readi32, buffer.writei32 } },
}
local function getn(t)
	return #t
end
-- ---- TypedArray base class ----
local TypedArray
do
	TypedArray = setmetatable({}, {
		__tostring = function()
			return "TypedArray"
		end,
	})
	TypedArray.__index = TypedArray
	function TypedArray.new(...)
		local self = setmetatable({}, TypedArray)
		return self:constructor(...) or self
	end
	function TypedArray:constructor(elementSize, signed, source, byteOffset, length)
		if byteOffset == nil then
			byteOffset = 0
		end
		self.BYTES_PER_ELEMENT = elementSize
		self.signed = signed
		self.byteOffset = byteOffset
		local _binding = bufferBySize[self.BYTES_PER_ELEMENT][(if self.signed then 1 else 0) + 1]
		local bufferRead = _binding[1]
		local bufferWrite = _binding[2]
		-- number -> allocate
		local _source = source
		if type(_source) == "number" then
			length = source
			self.byteLength = length * self.BYTES_PER_ELEMENT
			self.buffer = buffer.create(self.byteLength)
		else
			local _source_1 = source
			if type(_source_1) == "buffer" then
				local totalLen = buffer.len(source)
				self.buffer = source
				-- user provides explicit element length
				if length ~= nil then
					-- convert to bytes
					self.byteLength = length * self.BYTES_PER_ELEMENT
				else
					-- default: use remainder of buffer from byteOffset
					self.byteLength = totalLen - byteOffset
					length = self.byteLength / self.BYTES_PER_ELEMENT
				end
				-- validate bounds
				if byteOffset < 0 or byteOffset > totalLen then
					error(`byteOffset out of bounds`)
				end
				if byteOffset + self.byteLength > totalLen then
					error(`byteLength out of bounds`)
				end
				-- store final byteOffset and computed length
				self.byteOffset = byteOffset
			else
				local _source_2 = source
				if type(_source_2) == "table" then
					-- other typed array
					if source.buffer ~= nil and source.byteLength ~= nil then
						-- source instanceof TypedArray
						self.byteLength = source.byteLength
						length = math.floor(self.byteLength / self.BYTES_PER_ELEMENT)
						self.buffer = source.buffer
					else
						length = #source
						-- for (const v of source) arr.push(v);
						self.byteLength = length * self.BYTES_PER_ELEMENT
						self.buffer = buffer.create(self.byteLength)
						for i = 0, length - 1 do
							bufferWrite(self.buffer, i * self.BYTES_PER_ELEMENT, source[i + 1])
						end
					end
				else
					self.buffer = buffer.create(0)
					self.byteOffset = 0
					self.byteLength = 0
					length = 0
				end
			end
		end
		setmetatable(self, {
			__index = function(t, i)
				local index = tonumber(i)
				if index == nil then
					-- typeIs(index, "nil")
					local _condition = rawget(t, i)
					if _condition == nil then
						_condition = rawget(TypedArray, i)
					end
					return _condition
				end
				-- const off = t.byteOffset ?? 0 + idx * elementSize;
				local _, result = xpcall(function()
					return bufferRead(t.buffer, t.byteOffset + index * t.BYTES_PER_ELEMENT)
				end, function()
					return 0
				end)
				return result
			end,
			__newindex = function(t, i, v)
				local index = tonumber(i)
				if index == nil then
					return rawset(t, i, v)
				end
				local _condition = tonumber(v)
				if _condition == nil then
					_condition = 0
				end
				local value = _condition
				-- const off = t.byteOffset ?? 0 + idx * elementSize;
				bufferWrite(t.buffer, t.byteOffset + index * t.BYTES_PER_ELEMENT, value)
			end,
			__len = function()
				return length
			end,
		})
	end
	function TypedArray:subarray(startPos, endPos)
		if startPos == nil then
			startPos = 0
		end
		if endPos == nil then
			endPos = getn(self)
		end
		local length = getn(self)
		if startPos < 0 then
			startPos = math.max(length + startPos, 0)
		end
		if endPos < 0 then
			endPos = math.max(length + endPos, 0)
		end
		startPos = math.min(startPos, length)
		endPos = math.min(endPos, length)
		local newOffset = self.byteOffset + startPos * self.BYTES_PER_ELEMENT
		local newLength = endPos - startPos
		-- create a new TypedArray view on the same buffer
		return TypedArray.new(self.BYTES_PER_ELEMENT, self.signed, self.buffer, newOffset, newLength)
	end
	function TypedArray:set(source, offset)
		if offset == nil then
			offset = 0
		end
		local bufferWrite = bufferBySize[self.BYTES_PER_ELEMENT][(if self.signed then 1 else 0) + 1][2]
		local thisLength = getn(self)
		local sourceLength = getn(source)
		if offset < 0 or offset > thisLength then
			error("offset out of bounds")
		end
		if offset + sourceLength > thisLength then
			error("source does not fit")
		end
		local _source = source
		if type(_source) == "table" then
			if source.buffer ~= nil and source.byteOffset ~= nil and source.byteLength ~= nil then
				local srcStart = source.byteOffset
				local dstStart = self.byteOffset + offset * self.BYTES_PER_ELEMENT
				buffer.copy(self.buffer, dstStart, source.buffer, srcStart, source.byteLength)
			else
				for i = 0, sourceLength - 1 do
					local value = source[i + 1]
					bufferWrite(self.buffer, self.byteOffset + (offset + i) * self.BYTES_PER_ELEMENT, value)
				end
			end
		end
	end
	function TypedArray:slice(startPos, endPos)
		if startPos == nil then
			startPos = 0
		end
		if endPos == nil then
			endPos = getn(self)
		end
		local view = self:subarray(startPos, endPos)
		local out = TypedArray.new(view.BYTES_PER_ELEMENT, view.signed, getn(view))
		out:set(view)
		return out
	end
	function TypedArray:fill(value, startPos, endPos)
		if startPos == nil then
			startPos = 0
		end
		if endPos == nil then
			endPos = getn(self)
		end
		local len = getn(self)
		if startPos < 0 then
			startPos = math.max(len + startPos, 0)
		end
		if endPos < 0 then
			endPos = math.max(len + endPos, 0)
		end
		startPos = math.clamp(startPos, 0, len)
		endPos = math.clamp(endPos, 0, len)
		for i = startPos, endPos - 1 do
			self[i] = value
		end
		return self
	end
	function TypedArray:copyWithin(target, startPos, endPos)
		if endPos == nil then
			endPos = getn(self)
		end
		local len = getn(self)
		-- normalize negative numbers
		if target < 0 then
			target = len + target
		end
		if startPos < 0 then
			startPos = len + startPos
		end
		if endPos < 0 then
			endPos = len + endPos
		end
		target = math.clamp(target, 0, len)
		startPos = math.clamp(startPos, 0, len)
		endPos = math.clamp(endPos, 0, len)
		local count = math.min(endPos - startPos, len - target)
		if count <= 0 then
			return self
		end
		local elementSize = self.BYTES_PER_ELEMENT
		local srcOffset = self.byteOffset + startPos * elementSize
		local dstOffset = self.byteOffset + target * elementSize
		local byteLength = count * elementSize
		-- use buffer.copy, works even for overlapping regions
		buffer.copy(self.buffer, dstOffset, self.buffer, srcOffset, byteLength)
		--[[
			
					// temp buffer to preserve overlap semantics
					for (const i of $range(0, count - 1)) {
						this[target + i] = this[startPos + i];
					}
					
		]]
		return self
	end
end
-- ---- typed arrays ----
local Uint8Array
do
	local super = TypedArray
	Uint8Array = setmetatable({}, {
		__tostring = function()
			return "Uint8Array"
		end,
		__index = super,
	})
	Uint8Array.__index = Uint8Array
	function Uint8Array.new(...)
		local self = setmetatable({}, Uint8Array)
		return self:constructor(...) or self
	end
	function Uint8Array:constructor(source, byteOffset, length)
		super.constructor(self, 1, false, source, byteOffset, length)
	end
end
local Uint16Array
do
	local super = TypedArray
	Uint16Array = setmetatable({}, {
		__tostring = function()
			return "Uint16Array"
		end,
		__index = super,
	})
	Uint16Array.__index = Uint16Array
	function Uint16Array.new(...)
		local self = setmetatable({}, Uint16Array)
		return self:constructor(...) or self
	end
	function Uint16Array:constructor(source, byteOffset, length)
		super.constructor(self, 2, false, source, byteOffset, length)
	end
end
local Int16Array
do
	local super = TypedArray
	Int16Array = setmetatable({}, {
		__tostring = function()
			return "Int16Array"
		end,
		__index = super,
	})
	Int16Array.__index = Int16Array
	function Int16Array.new(...)
		local self = setmetatable({}, Int16Array)
		return self:constructor(...) or self
	end
	function Int16Array:constructor(source, byteOffset, length)
		super.constructor(self, 2, true, source, byteOffset, length)
	end
end
local Int32Array
do
	local super = TypedArray
	Int32Array = setmetatable({}, {
		__tostring = function()
			return "Int32Array"
		end,
		__index = super,
	})
	Int32Array.__index = Int32Array
	function Int32Array.new(...)
		local self = setmetatable({}, Int32Array)
		return self:constructor(...) or self
	end
	function Int32Array:constructor(source, byteOffset, length)
		super.constructor(self, 4, true, source, byteOffset, length)
	end
end
-- aliases for shorter compressed code (most minifers don't do this)
local u8 = Uint8Array
local u16 = Uint16Array
local i16 = Int16Array
local i32 = Int32Array
-- Huffman decoding table
-- FSE decoding table
-- decompress Zstandard state
local rb = function(d, b, n)
	local o = 0
	for i = 0, n - 1 do
		local _readable = o
		local _exp = d
		local _original = b
		b += 1
		o = bit32.bor(_readable, bit32.lshift(_exp[_original], (bit32.lshift(i, 3))))
	end
	--   for (; i < n; ++i) o |= d[b++] << (i << 3);
	return o
end
-- read Zstandard frame header
local rzfh = function(dat, w)
	local n3 = rb(dat, 0, 3)
	if tonumber(n3) == 0x2FB528 and tonumber(dat[3]) == 253 then
		-- Zstandard
		local flg = dat[4]
		--    single segment       checksum             dict flag     frame content flag
		local ss = bit32.band((bit32.arshift(flg, 5)), 1)
		local cc = bit32.band((bit32.arshift(flg, 2)), 1)
		local df = bit32.band(flg, 3)
		local fcf = bit32.arshift(flg, 6)
		local _value = bit32.band(flg, 8)
		if _value ~= 0 and _value == _value and _value then
			error("invalid zstd data")
		end
		-- byte
		local bt = 6 - ss
		-- dict bytes
		local db = if tonumber(df) == 3 then 4 else df
		-- dictionary id
		local di = rb(dat, bt, db)
		bt += db
		-- frame size bytes
		local fsb = if fcf ~= 0 and fcf == fcf and fcf then (bit32.lshift(1, fcf)) else ss
		-- frame source size
		local fss = rb(dat, bt, fsb) + (if ((tonumber(fcf) == 1) and 256) then 1 else 0)
		-- window size
		local ws = fss
		if not (ss ~= 0 and ss == ss and ss) then
			-- window descriptor
			local wb = bit32.lshift(1, (10 + (bit32.arshift(dat[5], 3))))
			ws = wb + (bit32.arshift(wb, 3)) * (bit32.band(dat[5], 7))
		end
		if ws > 2145386496 then
			error("window size too large (>2046MB)")
		end
		local _result
		if tonumber(w) == 1 then
			local _condition = fss
			if not (_condition ~= 0 and _condition == _condition and _condition) then
				_condition = ws
			end
			_result = _condition
		else
			_result = if w then 0 else ws
		end
		local buf = u8.new(_result + 12)
		buf[0] = 1
		local _exp = buf[0]
		buf[4] = 4
		local _exp_1 = buf[4]
		buf[8] = 8
		local _ = buf[8]
		return {
			b = bt + fsb,
			y = 0,
			l = 0,
			d = di,
			w = (if (w and tonumber(w) ~= 1) then w else buf:subarray(12)),
			e = ws,
			o = i32.new(buf.buffer, 0, 3),
			u = fss,
			c = cc,
			m = math.min(131072, ws),
		}
	elseif tonumber(bit32.bor((bit32.arshift(n3, 4)), (bit32.lshift(dat[3], 20)))) == 0x184D2A5 then
		-- skippable
		return rb(dat, 4, 4) + 8
	end
	error("invalid zstd data")
end
-- most significant bit for nonzero
local msb = function(val)
	local bits = 0
	while (bit32.lshift(1, bits)) <= val do
		bits += 1
	end
	return bits - 1
end
-- read finite state entropy
local rfse = function(dat, bt, mal)
	-- table pos
	local tpos = (bit32.lshift(bt, 3)) + 4
	-- accuracy log
	local al = (bit32.band(dat[bt], 15)) + 5
	if al > mal then
		error("FSE accuracy too high")
	end
	-- size
	local sz = bit32.lshift(1, al)
	-- probabilities symbols  repeat   index   high threshold
	local probs = sz
	local sym = -1
	local re = -1
	local i = -1
	local ht = sz
	-- optimization: single allocation is much faster
	local buf = buffer.create(512 + (bit32.lshift(sz, 2)))
	local freq = i16.new(buf, 0, 256)
	-- same view as freq
	local dstate = u16.new(buf, 0, 256)
	local nstate = u16.new(buf, 512, sz)
	local bb1 = 512 + (bit32.lshift(sz, 1))
	local syms = u8.new(buf, bb1, sz)
	local nbits = u8.new(buf, bb1 + sz)
	while sym < 255 and probs > 0 do
		local bits = msb(probs + 1)
		local cbt = bit32.arshift(tpos, 3)
		-- mask
		local msk = (bit32.lshift(1, (bits + 1))) - 1
		local val = bit32.band((bit32.arshift(rb(dat, cbt, 3), (bit32.band(tpos, 7)))), msk)
		-- mask (1 fewer bit)
		local msk1fb = (bit32.lshift(1, bits)) - 1
		-- max small value
		local msv = msk - probs - 1
		-- small value
		local sval = bit32.band(val, msk1fb)
		if sval < msv then
			tpos += bits
			local _exp = tpos
			val = sval
			local _ = val
		else
			tpos += bits + 1
			if val > msk1fb then
				val -= msv
			end
		end
		sym += 1
		val -= 1
		freq[sym] = val
		if tonumber(val) == -1 then
			probs += val
			ht -= 1
			syms[ht] = sym
		else
			probs -= val
		end
		if not (val ~= 0 and val == val and val) then
			repeat
				do
					-- repeat byte
					local rbt = bit32.arshift(tpos, 3)
					re = bit32.band((bit32.arshift(rb(dat, rbt, 2), (bit32.band(tpos, 7)))), 3)
					tpos += 2
					sym += re
				end
			until not (tonumber(re) == 3)
		end
	end
	local _value = sym > 255 or probs
	if _value ~= 0 and _value == _value and _value then
		error("invalid zstd data")
	end
	local sympos = 0
	-- sym step (coprime with sz - formula from zstd source)
	local sstep = (bit32.arshift(sz, 1)) + (bit32.arshift(sz, 3)) + 3
	-- sym mask
	local smask = sz - 1
	for s = 0, sym do
		local sf = freq[s]
		if sf < 1 then
			dstate[s] = -sf
			continue
		end
		-- This is split into two loops in zstd to avoid branching, but as JS is higher-level that is unnecessary
		for n = 0, sf - 1 do
			i = n
			syms[sympos] = s
			repeat
				do
					sympos = bit32.band((sympos + sstep), smask)
				end
			until not (sympos >= ht)
		end
	end
	-- After spreading symbols, should be zero again
	if sympos ~= 0 and sympos == sympos and sympos then
		error("invalid zstd data")
	end
	for n = 0, sz - 1 do
		i = n
		-- next state
		local _index = syms[i]
		local _original = dstate[_index]
		dstate[_index] += 1
		local ns = _original
		-- num bits
		nbits[i] = al - msb(ns)
		local nb = nbits[i]
		nstate[i] = (bit32.lshift(ns, nb)) - sz
	end
	return { bit32.arshift((tpos + 7), 3), {
		b = al,
		s = syms,
		n = nbits,
		t = nstate,
	} }
end
-- read huffman
local rhu = function(dat, bt)
	--  index  weight count
	local i = 0
	local wc = -1
	--    buffer             header byte
	local buf = u8.new(292)
	local hb = dat[bt]
	-- huffman weights
	local hw = buf:subarray(0, 256)
	-- rank count
	local rc = buf:subarray(256, 268)
	-- rank index
	local ri = u16.new(buf.buffer, 268)
	-- NOTE: at this point bt is 1 less than expected
	if hb < 128 then
		-- end byte, fse decode table
		local _binding = rfse(dat, bt + 1, 6)
		local ebt = _binding[1]
		local fdt = _binding[2]
		bt += hb
		local epos = bit32.lshift(ebt, 3)
		-- last byte
		local lb = dat[bt]
		if not (lb ~= 0 and lb == lb and lb) then
			error("invalid zstd data")
		end
		--  state1   state2   state1 bits   state2 bits
		local st1 = 0
		local st2 = 0
		local btr1 = fdt.b
		local btr2 = btr1
		-- fse pos
		-- pre-increment to account for original deficit of 1
		bt += 1
		local fpos = (bit32.lshift(bt, 3)) - 8 + msb(lb)
		while true do
			fpos -= btr1
			if fpos < epos then
				break
			end
			local cbt = bit32.arshift(fpos, 3)
			st1 += bit32.band((bit32.arshift(rb(dat, cbt, 2), (bit32.band(fpos, 7)))), ((bit32.lshift(1, btr1)) - 1))
			wc += 1
			hw[wc] = fdt.s[st1]
			fpos -= btr2
			if fpos < epos then
				break
			end
			cbt = bit32.arshift(fpos, 3)
			st2 += bit32.band((bit32.arshift(rb(dat, cbt, 2), (bit32.band(fpos, 7)))), ((bit32.lshift(1, btr2)) - 1))
			wc += 1
			hw[wc] = fdt.s[st2]
			btr1 = fdt.n[st1]
			st1 = fdt.t[st1]
			btr2 = fdt.n[st2]
			st2 = fdt.t[st2]
		end
		wc += 1
		if wc > 255 then
			error("invalid zstd data")
		end
	else
		wc = hb - 127
		while i < wc do
			local _exp = dat
			bt += 1
			local byte = _exp[bt]
			hw[i] = bit32.arshift(byte, 4)
			hw[i + 1] = bit32.band(byte, 15)
			i += 2
		end
		bt += 1
	end
	-- weight exponential sum
	local wes = 0
	for n = 0, wc - 1 do
		i = n
		local wt = hw[i]
		-- bits must be at most 11, same as weight
		if wt > 11 then
			error("invalid zstd data")
		end
		local _condition = wt
		if _condition ~= 0 and _condition == _condition and _condition then
			_condition = (bit32.lshift(1, (wt - 1)))
		end
		wes += _condition
	end
	-- max bits
	local mb = msb(wes) + 1
	-- table size
	local ts = bit32.lshift(1, mb)
	-- remaining sum
	local rem = ts - wes
	-- must be power of 2
	local _value = bit32.band(rem, (rem - 1))
	if _value ~= 0 and _value == _value and _value then
		error("invalid zstd data")
	end
	local _original = wc
	wc += 1
	hw[_original] = msb(rem) + 1
	for n = 0, wc - 1 do
		i = n
		local wt = hw[i]
		local _condition = wt
		if _condition ~= 0 and _condition == _condition and _condition then
			_condition = (mb + 1 - wt)
		end
		hw[i] = _condition
		rc[hw[i]] += 1
	end
	-- huf buf
	local hbuf = u8.new(bit32.lshift(ts, 1))
	--    symbols                      num bits
	local syms = hbuf:subarray(0, ts)
	local nb = hbuf:subarray(ts)
	ri[mb] = 0
	for n = mb, 1, -1 or 1 do
		i = n
		local pv = ri[i]
		local _exp = i
		local _index = i - 1
		ri[_index] = pv + rc[i] * (bit32.lshift(1, (mb - i)))
		nb:fill(_exp, pv, ri[_index])
	end
	if tonumber(ri[0]) ~= tonumber(ts) then
		error("invalid zstd data")
	end
	for n = 0, wc - 1 do
		i = n
		local bits = hw[i]
		if bits ~= 0 and bits == bits and bits then
			local code = ri[bits]
			local _exp = i
			ri[bits] = code + (bit32.lshift(1, (mb - bits)))
			syms:fill(_exp, code, ri[bits])
		end
	end
	--[[
		
			for (i = 0; i < wc; ++i) {
			  const bits = hw[i];
			  if (bits) {
			    const code = ri[bits];
			    syms.fill(i, code, ri[bits] = code + (1 << (mb - bits)));
			  }
			}
			
	]]
	return { bt, {
		n = nb,
		b = mb,
		s = syms,
	} }
end
-- Tables generated using this:
-- https://gist.github.com/101arrowz/a979452d4355992cbf8f257cbffc9edd
-- default literal length table
local dllt = rfse(u8.new({ 81, 16, 99, 140, 49, 198, 24, 99, 12, 33, 196, 24, 99, 102, 102, 134, 70, 146, 4 }), 0, 6)[2]
-- default match length table
local dmlt = rfse(u8.new({ 33, 20, 196, 24, 99, 140, 33, 132, 16, 66, 8, 33, 132, 16, 66, 8, 33, 68, 68, 68, 68, 68, 68, 68, 68, 36, 9 }), 0, 6)[2]
-- default offset code table
local doct = rfse(u8.new({ 32, 132, 16, 66, 102, 70, 68, 68, 68, 68, 36, 73, 2 }), 0, 5)[2]
-- bits to baseline
local b2bl = function(b, s)
	local len = getn(b)
	local bl = i32.new(len)
	for i = 0, len - 1 do
		bl[i] = s
		s += bit32.lshift(1, b[i])
	end
	return bl
end
-- literal length bits
local llb = u8.new((i32.new({ 0, 0, 0, 0, 16843009, 50528770, 134678020, 202050057, 269422093 })).buffer, 0, 36)
-- literal length baseline
local llbl = b2bl(llb, 0)
-- match length bits
local mlb = u8.new((i32.new({ 0, 0, 0, 0, 0, 0, 0, 0, 16843009, 50528770, 117769220, 185207048, 252579084, 16 })).buffer, 0, 53)
-- match length baseline
local mlbl = b2bl(mlb, 3)
-- decode huffman stream
local dhu = function(dat, out, hu)
	local len = getn(dat)
	local ss = getn(out)
	local lb = dat[len - 1]
	local msk = (bit32.lshift(1, hu.b)) - 1
	local eb = -hu.b
	if not (lb ~= 0 and lb == lb and lb) then
		error("invalid zstd data")
	end
	local st = 0
	local btr = hu.b
	local pos = (bit32.lshift(len, 3)) - 8 + msb(lb) - btr
	local i = -1
	while pos > eb and i < ss do
		local cbt = bit32.arshift(pos, 3)
		local val = (bit32.arshift(rb(dat, cbt, 3), (bit32.band(pos, 7))))
		st = bit32.band((bit32.bor((bit32.lshift(st, btr)), val)), msk)
		local _exp = out
		i += 1
		_exp[i] = hu.s[st]
		btr = hu.n[st]
		pos -= btr
	end
	if tonumber(pos) ~= tonumber(eb) or tonumber(i + 1) ~= tonumber(ss) then
		error("invalid zstd data")
	end
end
-- decode huffman stream 4x
-- TODO: use workers to parallelize
local dhu4 = function(dat, out, hu)
	local bt = 6
	local ss = getn(out)
	local sz1 = bit32.arshift((ss + 3), 2)
	local sz2 = bit32.lshift(sz1, 1)
	local sz3 = sz1 + sz2
	local _dat = dat
	local _exp = bt
	bt += rb(dat, 0, 2)
	dhu(_dat:subarray(_exp, bt), out:subarray(0, sz1), hu)
	local _dat_1 = dat
	local _exp_1 = bt
	bt += rb(dat, 2, 2)
	dhu(_dat_1:subarray(_exp_1, bt), out:subarray(sz1, sz2), hu)
	local _dat_2 = dat
	local _exp_2 = bt
	bt += rb(dat, 4, 2)
	dhu(_dat_2:subarray(_exp_2, bt), out:subarray(sz2, sz3), hu)
	dhu(dat:subarray(bt), out:subarray(sz3), hu)
end
-- read Zstandard block
local rzb = function(dat, st, out)
	local bt = st.b
	--    byte 0        block type
	local b0 = dat[bt]
	local btype = bit32.band((bit32.arshift(b0, 1)), 3)
	st.l = bit32.band(b0, 1)
	local sz = bit32.bor(bit32.bor((bit32.arshift(b0, 3)), (bit32.lshift(dat[bt + 1], 5))), (bit32.lshift(dat[bt + 2], 13)))
	-- end byte for block
	bt += 3
	local ebt = bt + sz
	if tonumber(btype) == 1 then
		if bt >= getn(dat) then
			return nil
		end
		st.b = bt + 1
		if out then
			local _out = out
			local _exp = dat[bt]
			local _exp_1 = st.y
			st.y += sz
			_out:fill(_exp, _exp_1, st.y)
			return out
		end
		return u8.new(sz):fill(dat[bt])
	end
	if ebt > getn(dat) then
		return nil
	end
	if tonumber(btype) == 0 then
		st.b = ebt
		if out then
			out:set(dat:subarray(bt, ebt), st.y)
			st.y += sz
			return out
		end
		return dat:slice(bt, ebt)
	end
	if tonumber(btype) == 2 then
		--    byte 3        lit btype     size format
		local b3 = dat[bt]
		local lbt = bit32.band(b3, 3)
		local sf = bit32.band((bit32.arshift(b3, 2)), 3)
		-- lit src size  lit cmp sz 4 streams
		local lss = bit32.arshift(b3, 4)
		local lcs = 0
		local s4 = 0
		if lbt < 2 then
			local _value = bit32.band(sf, 1)
			if _value ~= 0 and _value == _value and _value then
				local _readable = lss
				local _exp = dat
				bt += 1
				local _exp_1 = (bit32.lshift(_exp[bt], 4))
				local _condition = (bit32.band(sf, 2))
				if _condition ~= 0 and _condition == _condition and _condition then
					local _exp_2 = dat
					bt += 1
					_condition = (bit32.lshift(_exp_2[bt], 12))
				end
				lss = bit32.bor(_readable, bit32.bor(_exp_1, _condition))
			else
				lss = bit32.arshift(b3, 3)
			end
		else
			s4 = sf
			if sf < 2 then
				local _readable = lss
				local _exp = dat
				bt += 1
				lss = bit32.bor(_readable, (bit32.lshift((bit32.band(_exp[bt], 63)), 4)))
				local _exp_1 = lss
				local _exp_2 = (bit32.arshift(dat[bt], 6))
				local _exp_3 = dat
				bt += 1
				lcs = bit32.bor(_exp_2, (bit32.lshift(_exp_3[bt], 2)))
				local _ = lcs
			elseif tonumber(sf) == 2 then
				local _readable = lss
				local _exp = dat
				bt += 1
				local _exp_1 = (bit32.lshift(_exp[bt], 4))
				local _exp_2 = dat
				bt += 1
				lss = bit32.bor(_readable, bit32.bor(_exp_1, (bit32.lshift((bit32.band(_exp_2[bt], 3)), 12))))
				local _exp_3 = lss
				local _exp_4 = (bit32.arshift(dat[bt], 2))
				local _exp_5 = dat
				bt += 1
				lcs = bit32.bor(_exp_4, (bit32.lshift(_exp_5[bt], 6)))
				local _ = lcs
			else
				local _readable = lss
				local _exp = dat
				bt += 1
				local _exp_1 = (bit32.lshift(_exp[bt], 4))
				local _exp_2 = dat
				bt += 1
				lss = bit32.bor(_readable, bit32.bor(_exp_1, (bit32.lshift((bit32.band(_exp_2[bt], 63)), 12))))
				local _exp_3 = lss
				local _exp_4 = (bit32.arshift(dat[bt], 6))
				local _exp_5 = dat
				bt += 1
				local _exp_6 = bit32.bor(_exp_4, (bit32.lshift(_exp_5[bt], 2)))
				local _exp_7 = dat
				bt += 1
				lcs = bit32.bor(_exp_6, (bit32.lshift(_exp_7[bt], 10)))
				local _ = lcs
			end
		end
		bt += 1
		-- add literals to end - can never overlap with backreferences because unused literals always appended
		local buf = if out then out:subarray(st.y, st.y + st.m) else u8.new(st.m)
		-- starting point for literals
		local spl = getn(buf) - lss
		if tonumber(lbt) == 0 then
			local _buf = buf
			local _dat = dat
			local _exp = bt
			bt += lss
			_buf:set(_dat:subarray(_exp, bt), spl)
		elseif tonumber(lbt) == 1 then
			local _buf = buf
			local _exp = dat
			local _original = bt
			bt += 1
			_buf:fill(_exp[_original], spl)
		else
			-- huffman table
			local hu = st.h
			if tonumber(lbt) == 2 then
				local hud = rhu(dat, bt)
				-- subtract description length
				local _exp = bt
				bt = hud[1]
				lcs += _exp - bt
				hu = hud[2]
				st.h = hu
			elseif not hu then
				error("invalid zstd data")
			end
			local _fn = if s4 ~= 0 and s4 == s4 and s4 then dhu4 else dhu
			local _dat = dat
			local _exp = bt
			bt += lcs
			_fn(_dat:subarray(_exp, bt), buf:subarray(spl), hu)
		end
		-- num sequences
		local _exp = dat
		local _original = bt
		bt += 1
		local ns = _exp[_original]
		if ns ~= 0 and ns == ns and ns then
			if tonumber(ns) == 255 then
				local _exp_1 = dat
				local _original_1 = bt
				bt += 1
				local _exp_2 = _exp_1[_original_1]
				local _exp_3 = dat
				local _original_2 = bt
				bt += 1
				ns = (bit32.bor(_exp_2, (bit32.lshift(_exp_3[_original_2], 8)))) + 0x7F00
			elseif ns > 127 then
				local _exp_1 = (bit32.lshift((ns - 128), 8))
				local _exp_2 = dat
				local _original_1 = bt
				bt += 1
				ns = bit32.bor(_exp_1, _exp_2[_original_1])
			end
			-- symbol compression modes
			local _exp_1 = dat
			local _original_1 = bt
			bt += 1
			local scm = _exp_1[_original_1]
			local _value = bit32.band(scm, 3)
			if _value ~= 0 and _value == _value and _value then
				error("invalid zstd data")
			end
			local dts = { dmlt, doct, dllt }
			for i = 2, 0, -1 or 1 do
				local md = bit32.band((bit32.arshift(scm, ((bit32.lshift(i, 1)) + 2))), 3)
				if tonumber(md) == 1 then
					-- rle buf
					local _exp_2 = dat
					local _original_2 = bt
					bt += 1
					local rbuf = u8.new({ 0, 0, _exp_2[_original_2] })
					dts[i + 1] = {
						s = rbuf:subarray(2, 3),
						n = rbuf:subarray(0, 1),
						t = u16.new(rbuf.buffer, 0, 1),
						b = 0,
					}
				elseif tonumber(md) == 2 then
					-- accuracy log 8 for offsets, 9 for others
					local _binding = rfse(dat, bt, 9 - (bit32.band(i, 1)))
					bt = _binding[1]
					dts[i + 1] = _binding[2]
				elseif tonumber(md) == 3 then
					if not st.t then
						error("invalid zstd data")
					end
					dts[i + 1] = st.t[i + 1]
				end
			end
			st.t = dts
			local _binding = st.t
			local mlt = _binding[1]
			local oct = _binding[2]
			local llt = _binding[3]
			local lb = dat[ebt - 1]
			if not (lb ~= 0 and lb == lb and lb) then
				error("invalid zstd data")
			end
			local spos = (bit32.lshift(ebt, 3)) - 8 + msb(lb) - llt.b
			local cbt = bit32.arshift(spos, 3)
			local oubt = 0
			local lst = bit32.band((bit32.arshift(rb(dat, cbt, 2), (bit32.band(spos, 7)))), ((bit32.lshift(1, llt.b)) - 1))
			spos -= oct.b
			cbt = bit32.arshift(spos, 3)
			local ost = bit32.band((bit32.arshift(rb(dat, cbt, 2), (bit32.band(spos, 7)))), ((bit32.lshift(1, oct.b)) - 1))
			spos -= mlt.b
			cbt = bit32.arshift(spos, 3)
			local mst = bit32.band((bit32.arshift(rb(dat, cbt, 2), (bit32.band(spos, 7)))), ((bit32.lshift(1, mlt.b)) - 1))
			ns += 1
			while true do
				ns -= 1
				if not (ns ~= 0 and ns == ns and ns) then
					break
				end
				local llc = llt.s[lst]
				local lbtr = llt.n[lst]
				local mlc = mlt.s[mst]
				local mbtr = mlt.n[mst]
				local ofc = oct.s[ost]
				local obtr = oct.n[ost]
				spos -= ofc
				cbt = bit32.arshift(spos, 3)
				local ofp = bit32.lshift(1, ofc)
				local off = ofp + (bit32.band((bit32.rshift(rb(dat, cbt, 4), (bit32.band(spos, 7)))), (ofp - 1)))
				spos -= mlb[mlc]
				cbt = bit32.arshift(spos, 3)
				local ml = mlbl[mlc] + (bit32.band((bit32.arshift(rb(dat, cbt, 3), (bit32.band(spos, 7)))), ((bit32.lshift(1, mlb[mlc])) - 1)))
				spos -= llb[llc]
				cbt = bit32.arshift(spos, 3)
				local ll = llbl[llc] + (bit32.band((bit32.arshift(rb(dat, cbt, 3), (bit32.band(spos, 7)))), ((bit32.lshift(1, llb[llc])) - 1)))
				spos -= lbtr
				cbt = bit32.arshift(spos, 3)
				lst = llt.t[lst] + (bit32.band((bit32.arshift(rb(dat, cbt, 2), (bit32.band(spos, 7)))), ((bit32.lshift(1, lbtr)) - 1)))
				spos -= mbtr
				cbt = bit32.arshift(spos, 3)
				mst = mlt.t[mst] + (bit32.band((bit32.arshift(rb(dat, cbt, 2), (bit32.band(spos, 7)))), ((bit32.lshift(1, mbtr)) - 1)))
				spos -= obtr
				cbt = bit32.arshift(spos, 3)
				ost = oct.t[ost] + (bit32.band((bit32.arshift(rb(dat, cbt, 2), (bit32.band(spos, 7)))), ((bit32.lshift(1, obtr)) - 1)))
				if off > 3 then
					st.o[2] = st.o[1]
					st.o[1] = st.o[0]
					off -= 3
					st.o[0] = off
				else
					local idx = off - (tonumber(ll) ~= 0)
					if idx ~= 0 and idx == idx and idx then
						off = if tonumber(idx) == 3 then st.o[0] - 1 else st.o[idx]
						if idx > 1 then
							st.o[2] = st.o[1]
						end
						st.o[1] = st.o[0]
						st.o[0] = off
					else
						off = st.o[0]
					end
				end
				for i = 0, ll - 1 do
					buf[oubt + i] = buf[spl + i]
				end
				oubt += ll
				local _exp_2 = oubt
				spl += ll
				local _ = spl
				local stin = oubt - off
				if stin < 0 then
					local len = -stin
					local bs = st.e + stin
					if len > ml then
						len = ml
					end
					for i = 0, len - 1 do
						buf[oubt + i] = st.w[bs + i]
					end
					oubt += len
					local _exp_3 = oubt
					ml -= len
					local _exp_4 = ml
					stin = 0
					local _1 = stin
				end
				for i = 0, ml - 1 do
					buf[oubt + i] = buf[stin + i]
				end
				oubt += ml
			end
			if tonumber(oubt) ~= tonumber(spl) then
				while spl < getn(buf) do
					local _exp_2 = buf
					local _original_2 = oubt
					oubt += 1
					local _exp_3 = buf
					local _original_3 = spl
					spl += 1
					_exp_2[_original_2] = _exp_3[_original_3]
				end
			else
				oubt = getn(buf)
			end
			if out then
				st.y += oubt
			else
				buf = buf:slice(0, oubt)
			end
		elseif out then
			st.y += lss
			if spl ~= 0 and spl == spl and spl then
				for i = 0, lss - 1 do
					buf[i] = buf[spl + i]
				end
			end
		elseif spl ~= 0 and spl == spl and spl then
			buf = buf:slice(spl)
		end
		st.b = ebt
		return buf
	end
	error("invalid block type")
end
-- concat
local cct = function(bufs, ol)
	if tonumber(getn(bufs)) == 1 then
		return bufs[1]
	end
	local buf = u8.new(ol)
	local b = 0
	for i = 0, getn(bufs) - 1 do
		local chk = bufs[i + 1]
		buf:set(chk, b)
		b += getn(chk)
	end
	return buf
end
--[[
	*
	 * Decompresses Zstandard data
	 * @param data The input data
	 * @param decompressedSize The decompressed size. If unspecified, the function will allocate
	 *            exactly enough memory to fit the decompressed data. If your
	 *            data has multiple frames and you know the output size, specifying
	 *            it will yield better performance.
	 * @returns The decompressed data
	 
]]
local function decompress(data, decompressedSize)
	local _decompressedSize = decompressedSize
	local buf = if type(_decompressedSize) == "number" then Uint8Array.new(decompressedSize) else nil
	local dat = Uint8Array.new(data)
	local bufs = {}
	local nb = if not buf then 1 else 0
	local bt = 0
	local ol = 0
	while true do
		local _value = getn(dat)
		if not (_value ~= 0 and _value == _value and _value) then
			break
		end
		local _exp = dat
		local _condition = nb
		if not (_condition ~= 0 and _condition == _condition and _condition) then
			_condition = buf
		end
		local st = rzfh(_exp, _condition)
		if type(st) == "table" then
			if nb ~= 0 and nb == nb and nb then
				buf = nil
				if tonumber(getn(st.w)) == tonumber(st.u) then
					buf = st.w
					local _buf = buf
					table.insert(bufs, _buf)
					ol += st.u
				end
			else
				local _buf = buf
				table.insert(bufs, _buf)
				st.e = 0
			end
			while true do
				local _value_1 = st.l
				if not not (_value_1 ~= 0 and _value_1 == _value_1 and _value_1) then
					break
				end
				local blk = rzb(dat, st, buf)
				if not blk then
					error("unexpected EOF")
				end
				if buf then
					st.e = st.y
				else
					table.insert(bufs, blk)
					ol += getn(blk)
					st.w:copyWithin(0, getn(blk))
					st.w:set(blk, getn(st.w) - getn(blk))
				end
			end
			bt = st.b + (st.c * 4)
		else
			bt = st
		end
		dat = dat:subarray(bt)
	end
	local u8result = cct(bufs, ol)
	local result = buffer.create(u8result.byteLength)
	buffer.copy(result, 0, u8result.buffer, u8result.byteOffset, u8result.byteLength)
	return result
end
return {
	decompress = decompress,
}
