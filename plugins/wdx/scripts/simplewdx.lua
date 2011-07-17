-- Simple example of how to write wdx-scripts

function ContentSetDefaultParams(IniFileName,PlugApiVerHi,PlugApiVerLow)
  --Initialization code here
end

first=true;
function ContentGetSupportedField(Index)
  if (not first) then 
    return '','', 0; -- ft_nomorefields
  end 

  if (first) then
    first=false;
    return 'FieldName','', 8; -- FieldName,Units,ft_string
  end  
end

function ContentGetDefaultSortOrder(FieldIndex)
  return 1; --or -1
end

function ContentGetDetectString()
  return 'EXT="TXT"'; -- return detect string
end

function ContentGetValue(FileName, FieldIndex, UnitIndex, flags)
return "test"; --return string
end

--function ContentGetSupportedFieldFlags(FieldIndex)
  --return 0; -- return flags
--end

--function ContentStopGetValue(Filename)
--end
