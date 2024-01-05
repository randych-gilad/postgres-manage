local date = os.date("%Y%m%d")
local isAlreadyExisting = function()
  local function trim_find(find)
    return find:gsub("./", "")
  end
  local function trim_table(find)
    local mytable = {}
    for line in find:gmatch("[^\r\n]+") do
      line = trim_find(line)
      table.insert(mytable, line)
    end
    return mytable
  end
  local exist_first = io.popen(string.format("find . -mindepth 1 -type d -name '%s'", date)):read("*a")
  exist_first = trim_find(exist_first):gsub("\n", "")
  local exist_next = io.popen(string.format("find . -mindepth 1 -type d -name '%s.*'", date)):read("*a")
  exist_next = trim_table(exist_next)
  return exist_first, exist_next
end
