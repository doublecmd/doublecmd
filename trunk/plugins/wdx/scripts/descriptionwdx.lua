-- This script reads file descriptions from descript.ion

function ContentGetSupportedField(Index)
  if (Index > 0) then
    return '','', 0; -- ft_nomorefields
  end 

  return 'Description','', 8; -- FieldName,Units,ft_string
end

function ContentGetDefaultSortOrder(FieldIndex)
  return 1; --or -1
end

function ContentGetDetectString()
  return 'EXT="*"'; -- return detect string
end

function ContentGetValue(FileName, FieldIndex, UnitIndex, flags)
 if FieldIndex==0 then
   --Linux paths only
   local pat="/.*/"
   i,j=string.find(FileName,pat);
   if i~=nil then
     local path=string.sub(FileName,i,j);
     fn=string.sub(FileName,string.len(path)+1,-1);
     if fn~=".." then
       return GetDesc(path,fn);
     else 
       return "";
     end  
   end
 end
 return nil;
end

function GetDesc(Path,Name)
   local f=io.open(Path..'descript.ion',"r");
   if not f then 
    return nil;
   end
  
    for line in f:lines() do
       if string.find(line,Name..' ') then
        f:close();
	return string.sub(line,string.len(Name..' ')+1,-1);
       end
    end  

  f:close();
     
  return nil;
end
