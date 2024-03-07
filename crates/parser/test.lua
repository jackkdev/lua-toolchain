local t = { 1, 2; 3 }
local var1 = 0 + 0.0 - 0.0e-1 / 0x0 * 0x0.0 % 000000.1234500000 ^ 0.31416E1 + #t + t.a + t["a"] + t:a()
local var2 = "Hello, world!" .. [[Hello, world!]] .. [=[Hello, world!]=] .. [====[Hello, world!]====]
local var3 = not (true == false) and (0 >= 0) or (0 <= 0) and (0 > 0) or (0 < 0) or (false ~= true)

if t then
elseif t + 1 then
else
end

for k, v in ipairs(t) do
    break
end

while true do
    return
end

repeat
until true

function example(v1, v2, v3)
end