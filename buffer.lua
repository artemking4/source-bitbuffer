---@diagnostic disable: param-type-mismatch

local ffi = require "ffi"
local int64, uint64 = 
    ffi.typeof "long long",
    ffi.typeof "unsigned long long"

local buffer = { }

---@class BufferCursor
---@field byte integer
---@field bit integer
---@field advance fun(self: BufferCursor, bits: integer)
---@field ceil fun(self: BufferCursor)

---@class SourceBuffer
---@field _bytecache integer[]
---@field data integer[]
---@field cursor BufferCursor
---@field toString fun(self: SourceBuffer):string
---@field didOverflow fun(self: SourceBuffer):boolean

---@param data? string
---@return SourceBuffer
local function basebuf(data)
    local buf = {
        data = data and { data:byte(1, #data) } or { },

        cursor = {
            byte = 1,
            bit = 0
        },

        _bytecache = { }
    }

    function buf.cursor:advance(bits)
        self.byte = self.byte + math.floor((self.bit + bits) / 8)
        self.bit = (self.bit + bits) % 8
    end
    
    function buf.cursor:ceil()
        if self.bit > 0 then 
            self.byte = self.byte + 1
        end
        self.bit = 0
    end

    function buf:didOverflow()
        return (self.cursor.byte + math.ceil(self.cursor.bit / 8) - 1) > #data
    end

    function buf:toString()
        local t = { }
        for k,v in ipairs(self.data) do 
            t[#t + 1] = string.char(v)
        end

        return table.concat(t)
    end

    return buf
end

---@class SourceReadBuffer : SourceBuffer
---@field getBytes fun(self: SourceReadBuffer, bytes: integer, bits?: integer):integer[] -- for internal use only
---@field readBit fun(self: SourceReadBuffer):integer
---@field readByte fun(self: SourceReadBuffer):integer
---@field readBool fun(self: SourceReadBuffer, byte?: boolean):boolean
---@field readUInt fun(self: SourceReadBuffer, width: integer):integer
---@field readUInt16 fun(self: SourceReadBuffer):integer
---@field readUInt32 fun(self: SourceReadBuffer):integer
---@field readUInt64 fun(self: SourceReadBuffer):integer
---@field readInt fun(self: SourceReadBuffer, width: integer):integer
---@field readInt16 fun(self: SourceReadBuffer):integer
---@field readInt32 fun(self: SourceReadBuffer):integer
---@field readChar fun(self: SourceReadBuffer):string
---@field readString fun(self: SourceReadBuffer):string
---@field readFloat32 fun(self: SourceReadBuffer):string

---@param data? string
---@return SourceReadBuffer
function buffer.read(data)
    local buf = basebuf(data)
    ---@cast buf SourceReadBuffer

    local function getByte(b1, b2, boff)
        local d = bit.bor(bit.lshift(b2, 8), b1)
        d = bit.rshift(d, boff)
        return bit.band(d, 0xFF)
    end

    -- returns bytes in big-endian order, bit-shifted
    function buf:getBytes(amount, bits)
        bits = bits or amount * 8

        for i = 0, amount - 1 do
            local b1, b2 =
                self.data[self.cursor.byte + i],
                self.data[self.cursor.byte + i + 1]

            if (not b2 and (7 - bits % 8) >= (8 - self.cursor.bit))
                or not b1 then 
                return false
            end

            local boff = self.cursor.bit
            local bo = getByte(b1, b2 or 0, boff)
            if i == amount - 1 and bits % 8 ~= 0 then 
                bo = bit.band(bit.lshift(1, bits % 8) - 1, bo) -- throw away bits higher than requested 
            end

            self._bytecache[amount - i] = bo
        end
        self._bytecache[amount + 1] = nil

        return self._bytecache
    end

    function buf:readBit()
        local byte = self.data[self.cursor.byte]
        if not byte then return 0, true end
        local bit = bit.band(2 ^ (7 - self.cursor.bit), byte)
        self.cursor:advance(1)
        return bit ~= 0 and 1 or 0
    end

    function buf:readBool(byte)
        local n
        if byte then 
            n = self:readByte()
        else
            n = self:readBit()
        end

        return n ~= 0
    end

    function buf:readByte()
        local bytes = self:getBytes(1)
        if not bytes then return 0, true end
        local byte = bytes[1]
        self.cursor:advance(8)
        return byte
    end

    function buf:readUInt(width)
        local bw = math.ceil(width / 8)
        local bytes = self:getBytes(bw, width)
        if not bytes then return 0, true end
        
        local n = 0

        local lastbyte = #bytes - 1
        for i = 0, lastbyte do 
            local byte = bytes[(lastbyte - i) + 1]
            n = bit.bor(n, bit.lshift(byte, i * 8))
        end

        self.cursor:advance(width)

        return n
    end

    function buf:readUInt16()
        local bytes = self:getBytes(2)
        if not bytes then return 0, true end
        self.cursor:advance(16)

        return bit.bor(
            bit.lshift(bytes[1], 8),
            bytes[2]
        )
    end

    function buf:readUInt32()
        local bytes = self:getBytes(4)
        if not bytes then return 0, true end
        self.cursor:advance(32)

        local n = bit.bor(
            bit.lshift(bytes[1], 24),
            bit.lshift(bytes[2], 16),
            bit.lshift(bytes[3], 8),
            bytes[4]
        )

        if n < 0 then 
            n = 0xFFFFFFFF - n - 1
        end

        return n
    end

    function buf:readUInt64()
        return bit.bor(self:readUInt32(), uint64(self:readUInt32()) * 2 ^ 32)
    end

    function buf:readInt(width)
        local n = self:readUInt(width)
        local msb = bit.lshift(1, width - 1)
        
        local sign = n > (msb - 1)
        if sign then
            n = -(msb * 2 - n)
        end

        return n
    end

    function buf:readInt16()
        local n = self:readUInt16()
        local sign = n > 2 ^ 15

        if sign then 
            n = n - 2 ^ 15
            n = -(2 ^ 15 - n)
        end

        return n
    end

    function buf:readInt32()
        local n = self:readUInt32()
        local sign = n > 2 ^ 31

        if sign then 
            n = n - 2 ^ 31
            n = -(2 ^ 31 - n)
        end

        return n
    end

    -- float funcs are stolen, idk how all the floating point shit works in memory
    function buf:readFloat32()
        local bytes = self:getBytes(4)
        if not bytes then return 0, true end
        self.cursor:advance(32)

        local b0 = bytes[1]
        local b1 = bytes[2]
        local sign = bit.band(b0, 128) ~= 0
        local exponent = bit.band(bit.lshift(b0, 1), 255) + bit.rshift(b1, 7)
        local mantissa = bit.lshift(bit.band(b1, 127), 23 - 7)
            + bit.lshift(bytes[3], 23 - 7 - 8)
            + bit.lshift(bytes[4], 23 - 7 - 8 - 8)

        if exponent == 255 then -- 2^8-1
            if mantissa ~= 0 then
                return 0 / 0
            else
                return sign and -math.huge or math.huge
            end
        elseif exponent == 0 then
            if mantissa == 0 then
                return 0
            else
                -- -126 is the 0-bias+1
                return sign and -math.ldexp(mantissa / 8388608, -126) or math.ldexp(mantissa / 8388608, -126)
            end
        end

        mantissa = (mantissa / 8388608) + 1

        return sign and -math.ldexp(mantissa, exponent - 127) or math.ldexp(mantissa, exponent - 127)
    end

    function buf:readChar()
        return string.char(self:readByte())
    end

    function buf:readString(len, linebreaks)
        len = len or 2048 -- default for ReadAndAllocateString
        
        local t = { }
        while true do
            local c = self:readChar()
            if c == "\0" then 
                break
            elseif linebreaks and c == "\n" then
                break
            end

            if len > 0 then
                t[#t + 1] = c
                len = len - 1
            end
        end

        return table.concat(t)
    end

    return buf
end

---@class SourceWriteBuffer : SourceBuffer
---@field writeBit fun(self: SourceReadBuffer, bit: integer)
---@field _writeBitsOfByte fun(self: SourceReadBuffer, byte: integer, bits: integer)
---@field writeByte fun(self: SourceReadBuffer, byte: integer)
---@field writeBytes fun(self: SourceReadBuffer, bytes: integer[], bits?: integer)
---@field writeChar fun(self: SourceReadBuffer, char: string)
---@field writeString fun(self: SourceReadBuffer, str: string)
---@field writeUInt fun(self: SourceReadBuffer, n: integer, width: integer)
---@field writeUInt16 fun(self: SourceReadBuffer, n: integer)
---@field writeUInt32 fun(self: SourceReadBuffer, n: integer)
---@field writeInt fun(self: SourceReadBuffer, n: integer, width: integer)
---@field writeInt16 fun(self: SourceReadBuffer, n: integer)
---@field writeInt32 fun(self: SourceReadBuffer, n: integer)
---@field writeFloat32 fun(self: SourceReadBuffer, n: number)

function buffer.write()
    local buf = basebuf()
    ---@cast buf SourceWriteBuffer

    function buf:writeBit(n)
        local byte = self.data[self.cursor.byte] or 0
        if n ~= 0 then 
            self.data[self.cursor.byte] = bit.bor(byte, 2 ^ (7 - self.cursor.bit))
        else
            self.data[self.cursor.byte] = byte
        end
        self.cursor:advance(1)
    end

    function buf:_writeBitsOfByte(byte, bits)
        if bits ~= 8 then
            byte = bit.band(bit.lshift(1, bits) - 1, byte)
        end

        local bl, bh = 
            self.data[self.cursor.byte] or 0,
            self.data[self.cursor.byte + 1] or 0

        local combined = bit.bor(bit.lshift(bh, 8), bl)
        local offbyte = bit.lshift(byte, self.cursor.bit)
        
        combined = bit.bor(combined, offbyte)
        bh, bl = 
            bit.rshift(combined, 8),
            bit.band(combined, 0xFF)

        self.data[self.cursor.byte] = bl
        if (7 - bits % 8) >= (8 - self.cursor.bit) then
            self.data[self.cursor.byte + 1] = bh
        end

        self.cursor:advance(bits)
    end

    function buf:writeByte(byte)
        self:_writeBitsOfByte(byte, 8)
    end

    -- get bytes in big endian and write them (limited by bitcount)
    function buf:writeBytes(bytes, bitcount)
        bitcount = bitcount or bytes * 8
        local fb = #bytes

        for i = 0, fb - 1 do 
            local byte = bytes[fb - i]
            local size = 8

            if i == fb - 1 and bitcount % 8 ~= 0 then -- we need to supply a lower size to the most significant byte
                size = bitcount % 8
            end

            self:_writeBitsOfByte(byte, size)
        end
    end

    function buf:writeBool(val, byte)
        if byte then 
            self:writeByte(val and 1 or 0)
        else
            self:writeBit(val and 1 or 0)
        end
    end

    function buf:writeChar(char)
        self:writeByte(char:byte())
    end

    function buf:writeString(str)
        for i = 1, #str do 
            self:writeChar(str:sub(i, i))
        end
        self:writeByte(0)
    end

    function buf:writeUInt(n, width)
        local bytes = math.ceil(width / 8)
        local bc = self._bytecache

        for i = 0, bytes - 1 do 
            bc[i + 1] = bit.band(0xFF, bit.rshift(n, (bytes - i - 1) * 8))
        end
        bc[bytes + 1] = nil

        self:writeBytes(bc, width)
    end

    function buf:writeUInt16(n)
        local bc = self._bytecache

        bc[1] = bit.band(0xFF, bit.rshift(n, 8))
        bc[2] = bit.band(0xFF, n)
        bc[3] = nil

        self:writeBytes(bc, 16)
    end

    function buf:writeUInt32(n)
        local bc = self._bytecache

        bc[1] = bit.band(0xFF, bit.rshift(n, 24))
        bc[2] = bit.band(0xFF, bit.rshift(n, 16))
        bc[3] = bit.band(0xFF, bit.rshift(n, 8))
        bc[4] = bit.band(0xFF, n)
        bc[5] = nil

        self:writeBytes(bc, 32)
    end

    function buf:writeInt(n, width)
        if n < 0 then 
            n = n + 2 ^ width
        end

        self:writeUInt(n, width)
    end

    function buf:writeInt16(n)
        if n < 0 then 
            n = n + 2 ^ 16
        end
        
        self:writeUInt(n, 16)
    end

    function buf:writeInt32(n)
        if n < 0 then 
            n = n + 2 ^ 32
        end

        self:writeUInt(n, 32)
    end

    function buf:writeFloat32(n)
        local bc = self._bytecache
        
        local sign = n < 0
        n = math.abs(n)

        local mantissa, exponent = math.frexp(n)

        if n == math.huge then
            if sign then
                bc[1] = 255
            else
                bc[1] = 255
            end

            bc[2] = 128
            bc[3] = 0
            bc[4] = 0
        elseif n ~= n then
            bc[1] = 127
            bc[2] = 255
            bc[3] = 255
            bc[4] = 255
        elseif n == 0 then
            bc[1] = 0
            bc[2] = 0
            bc[3] = 0
            bc[4] = 0
        elseif exponent + 127 <= 1 then -- bias for singles is 127
            mantissa = math.floor(mantissa * 8388608 + 0.5)

            if sign then
                bc[1] = 128 -- Sign bit, 7 empty bits for exponent
            else
                bc[1] = 0
            end

            bc[2] = bit.rshift(mantissa, 16)
            bc[3] = bit.band(bit.rshift(mantissa, 8), 255)
            bc[4] = bit.band(mantissa, 255)
        else
            mantissa = math.floor((mantissa - 0.5) * 16777216 + 0.5)

            -- 127-1 = 126
            if sign then -- sign + 7 exponent
                bc[1] = 128 + bit.rshift(exponent + 126, 1)
            else
                bc[1] = bit.rshift(exponent + 126, 1)
            end

            bc[2] = bit.band(bit.lshift(exponent + 126, 7), 255) + bit.rshift(mantissa, 16) -- 1 exponent + 7 mantissa
            bc[3] = bit.band(bit.rshift(mantissa, 8), 255) -- 8 mantissa
            bc[4] = bit.band(mantissa, 255) -- 8 mantissa
        end

        bc[5] = nil
        self:writeBytes(bc, 32)
    end

    return buf
end

---@cast buffer +fun(data?: string): SourceWriteBuffer|SourceReadBuffer
setmetatable(buffer, {
    __call = function(t, data)
        if data then 
            return buffer.read(data)
        else
            return buffer.write()
        end
    end
})

return buffer