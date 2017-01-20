-- Finds text in OpenDocument Text (.odt)
-- Requires: odt2txt tool

function ContentGetSupportedField(Index)
  if (Index == 0) then
    return 'Text','', 9; -- FieldName,Units,ft_fulltext
  end
  return '','', 0; -- ft_nomorefields
end

function ContentGetDetectString()
  return '(EXT="ODT")'; -- return detect string
end

function ContentGetValue(FileName, FieldIndex, UnitIndex, flags)

  if (FieldIndex > 0) then
    return nil;
  end

  if (UnitIndex == 0) then
    local f = io.popen ("odt2txt " .. FileName, 'r')

    if not f then
      return nil;
    end

    local ss = f:read("*a")
    f:close()

    return ss;
  end;

  return nil;
end
