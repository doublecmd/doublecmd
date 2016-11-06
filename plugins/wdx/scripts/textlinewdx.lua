
function ContentGetSupportedField(Index)
  if (Index == 0) then
    return 'Line1','', 8; -- FieldName,Units,ft_string
  elseif (Index == 1) then
    return 'Line2','', 8;
  elseif (Index == 2) then
    return 'Line3','', 8;
  elseif (Index == 3) then
    return 'Line4','', 8;
  elseif (Index == 4) then
    return 'Line5','', 8;
  end
  return '','', 0; -- ft_nomorefields
end

function ContentGetDetectString()
  return '(EXT="TXT") | (EXT="INI")'; -- return detect string
end

function ContentGetValue(FileName, FieldIndex, UnitIndex, flags)

  if (FieldIndex > 4) then
    return nil;
  end

   local f=io.open(FileName,"r");
   if not f then
    return nil;
   end
    local ii = 0;
    for line in f:lines() do
       if (ii == FieldIndex) then
        f:close();
	return line;
       end
       ii = ii + 1;
    end

  f:close();

  return nil;
end
