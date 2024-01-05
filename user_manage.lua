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
