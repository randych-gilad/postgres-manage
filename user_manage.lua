Databases = {} -- insert database list here

function ValidateUsername()
  while true do
    io.write("\n" .. "Enter username: ")
    local user = io.read()
    if string.find(user, "postgres") then
      io.stderr:write("ERROR: username cannot contain 'postgres'")
    else
      return string.lower(user)
    end
  end
end

function ValidatePassword()
  while true do
    io.write("Enter password: ")
    local password = io.read()
    if #password < 9 then
      io.stderr:write("ERROR: password too short\n")
    else
      return password
    end
  end
end

function VerifyResult(user, file_name)
  local file = assert(io.open(file_name, "r"))
  local content = file:read("*all")
  print("\n" .. content)
  file:close()
  io.write("Is this OK? (y/n) ")
  while true do
    local input = io.read()
    if input == "y" then
      print(string.format("Created .sql script for %s.\n", user))
      break
    elseif input == "n" then
      os.remove(file_name)
      print(string.format("Discarded .sql script for %s.\n", user))
      return
    else
      print("Only 'y' or 'n' is accepted.")
      input = io.read()
    end
  end

  io.write("Apply? (y/n) ")
  local command = string.format("psql -U postgres -f %s", file_name)
  while true do
    local input = io.read()
    if input == "y" then
      print()
      os.execute(command)
      print()
      break
    elseif input == "n" then
      os.remove(file_name)
      print(string.format("Discarded .sql script for %s.\n", user))
      break
    else
      print("Only 'y' or 'n' is accepted.")
    end
  end
end
