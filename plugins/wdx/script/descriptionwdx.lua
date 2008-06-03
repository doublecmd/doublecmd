#!/usr/local/bin/lua 

first=true;
function ContentGetSupportedField(Index)
  if (not first) then 
    return '','', 0; -- ft_nomorefields
  end 

  if (first) then
    first=false;
    return 'Description','', 8; -- FieldName,Units,ft_string
  end  
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
end


function GetDesc(Path,Name)
   local f=io.open(Path..'descript.ion',"r");
   if not f then 
    return "";
   end
  
    local CurrDesc={};
    for line in f:lines() do
      table.insert(CurrDesc,line);
    end  

    for i,v in pairs(CurrDesc) do
       if string.find(v,Name..' ') then
	return string.sub(v,string.len(Name..' ')+1,-1);
       end
    end

  CurrDesc=nil;
  f:close();
     
  return ds;
end

