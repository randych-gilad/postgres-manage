local date = os.date("%Y%m%d")
local isAlreadyExisting = function()
  local function trim_find(find)
    return find:gsub("./", "")
  end
  local function trim_table(find)
    local result = {}
    for line in find:gmatch("[^\r\n]+") do
      line = trim_find(line)
      table.insert(result, line)
    end
    return result
  end

  local exist_first = io.popen(string.format("find . -mindepth 1 -type d -name '%s'", date)):read("*a")
  local exist_more = io.popen(string.format("find . -mindepth 1 -type d -name '%s.*'", date)):read("*a")
  exist_first = trim_find(exist_first):gsub("\n", "")
  exist_more = trim_table(exist_more)
  return exist_first == "" and 0 or 1, #exist_more
end
function CreatePgDumpFolder()
  local first, more = isAlreadyExisting()
  local total_backups = first + more
  local MAX_BACKUPS = 5
  print("Total backups: " .. total_backups)
  print("Retention setting: " .. MAX_BACKUPS)
  local function create_folder()
    os.execute(string.format("mkdir %s", date))
    print(string.format("Created backup folder %s.", date))
  end
  local function create_folder_newer()
    if more > 0 then
      for i = more - 1, 0, -1 do
        os.rename(
          string.format("%s.%s", date, i),
          string.format("%s.%s", date, i + 1))
        print(string.format("Renamed folder %s.%s -> %s.%s", date, i, date, i + 1))
      end
    end
    os.rename(
      string.format("%s", date),
      string.format("%s.0", date))
    create_folder()
    print(string.format("Renamed older backup to %s.0.", date))
  end
  local function shift_cleanup()
    create_folder_newer()
    os.execute(string.format("rm -rf %s.%s", date, more))
    print(string.format("Removed 1 oldest backup: %s.%s", date, more))
  end

  if first == 0 then
    create_folder()
  elseif total_backups < MAX_BACKUPS then
    create_folder_newer()
  elseif total_backups == MAX_BACKUPS then
    shift_cleanup()
  end
end
