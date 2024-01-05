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
  local function count_lines(find)
    local count = 0
    if find == nil then return 0 end
    for _ in find:gmatch("[^\r\n]+") do
      count = count + 1
    end
    return count
  end
  local exist_first = io.popen(string.format("find . -mindepth 1 -type d -name '%s'", date)):read("*a")
  exist_first = trim_find(exist_first):gsub("\n", "")
  local exist_more = io.popen(string.format("find . -mindepth 1 -type d -name '%s.*'", date)):read("*a")
  exist_more = trim_table(exist_more)
  return count_lines(exist_first), #exist_more
end
function CreatePgDumpFolder()
  local first, more = isAlreadyExisting()
  print(first, more)
  local total_backups = first + more
  print("Total backups: " .. total_backups)
  local MAX_BACKUPS = 5
  local function create_folder()
    os.execute(string.format("mkdir %s", date))
    print(string.format("Created backup folder %s.", date))
    do return end
  end
  local function create_folder_newer()
    os.execute(string.format("mkdir %s.%s", date, more))
    print(string.format("Created backup folder %s.%s", date, more))
    do return end
  end
  local function shift_cleanup()
    os.execute(string.format("rm -rf %s", date))
    os.execute(string.format("mv %s.0 %s", date, date))
    for i = 1, 3 do
      os.execute(string.format("mv %s.%s %s.%s", date, i, date, i - 1))
    end
    print("Removed 1 oldest backup.")
    do return end
  end

  if first == 0 then
    create_folder()
  elseif total_backups < MAX_BACKUPS then
    create_folder_newer()
  elseif total_backups == MAX_BACKUPS then
    shift_cleanup()
  end
end
