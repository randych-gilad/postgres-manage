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
