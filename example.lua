local a = 10

local function do_something(...)
  local args = {...}

  if #args > 1 then
    args[1] = 0
  elseif #args > 2 then
    args[2] = 0
  else
    args = { 0, 1, 2, 3, "example" }
  end

  while true do
    if not args[1] == 5 and args[1] > 5 or args[2] < 3 then
      break
    end
  end

  for key, value in ipairs(args) do
    if key ~= 0 then
      break
    end
  end
end

local test = 0001