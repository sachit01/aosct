-- GLOBAL

local utilwireshark = {  }

Stack = {}

-- Create a Table with stack functions
function Stack:Create()

  local t = {}
  t._et = {}

  function t:top(...)
     local v = self:pop(1);
     self:push(v);
     return v;
  end

  function t:push(...)
    if ... then
      local targs = {...}
      for _,v in ipairs(targs) do
        table.insert(self._et, v)
      end
    end
  end

  function t:pop(num)
    local num = num or 1
    local entries = {}
    for i = 1, num do
      if #self._et ~= 0 then
        table.insert(entries, self._et[#self._et])
        table.remove(self._et)
      else
        break
      end
    end
    return unpack(entries)
  end

  function t:getn()
    return #self._et
  end

  function t:list()
    for i,v in pairs(self._et) do
      print(i, v)
    end
  end

  return t
end

utilwireshark.stack = Stack

function bitfield_add(buf, subtree, offset, size, bitfieldstart, bitfieldlen, name, valuestring, desc, format)
    local val = buf(offset, size):bitfield(bitfieldstart, bitfieldlen)
    local binary=""
    local i = bitfieldlen - 1
    local intval = val

    repeat
        local result = intval % 2^i
        if result ~= intval then
            binary = binary.."1"
        else
            binary = binary.."0"
        end
        intval = result
        i = i - 1
    until (i < 0)

    local dotted = string.rep(".", bitfieldstart)..binary
      ..string.rep(".", size*8-bitfieldstart-bitfieldlen)

    local chunked = ""
    for i = 0, size do
        chunked = chunked..string.sub(dotted, 8*i+1, 8*i+4)
          .." "..string.sub(dotted, 8*i+5, 8*i+8).." "
    end

    desc = chunked.." = "..desc
    if format ~= nil then
        desc = desc..": "..string.format(format, val)
    end

    if valuestring ~= nil then
        if valuestring[val] ~= nil then
            desc = desc.." ["..valuestring[val].."]"
        end
    end

    subtree:add(ProtoField.uint8(name), buf(offset, size), desc)
end

return utilwireshark;
