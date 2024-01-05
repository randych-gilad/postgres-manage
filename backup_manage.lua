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
  if #exist_first == 0 then exist_first = nil end
  if #exist_next == 0 then exist_next = nil end
  return exist_first, exist_next
end
function CreatePgDumpFolder()
  local first, next = isAlreadyExisting()
  local total_backups = #next + 1
  local MAX_BACKUPS = 10
  if not first then
    os.execute(string.format("mkdir %s", date))
  end
  if not first and next then
    os.execute(string.format("mkdir %s.1", date))
  end
  if total_backups == MAX_BACKUPS then
    print("Removed 1 oldest backup.")
    os.execute(string.format("rm -rf %s.*", date))
    os.execute(string.format("mv %s %s.%s", date, date, total_backups))
    os.execute(string.format("mkdir %s", date))
  elseif next then
    print(string.format("There is already %s backups for %s.", total_backups, date))
    print(string.format("Created backup %s.", total_backups + 1))
  end
end
