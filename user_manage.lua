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
