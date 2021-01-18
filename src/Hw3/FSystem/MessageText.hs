module Hw3.FSystem.MessageText 
  ( -- * System messages
    msgDirAlreadyExist
  , msgDirDoesn'tExist
  , msgDirOrFileDoesn'tExist
  , msgExit
  , msgFilAlreadyExist
  , msgFilDoesn'tExist
  , msgHelp
  )
where

-----------------------------------------
-- system messages are collected here  --
-----------------------------------------

msgHelp :: String
msgHelp =
 "cd <folder>                   -- перейти в директорию \n\
 \dir                           -- показать содержимое текущей директории \n\
 \ls <folder>                   -- показать содержимое выбранной директории \n\
 \create-folder \"folder-name\"   -- создать директорию в текущей \n\
 \cat <file>                    -- показать содержимое файла \n\
 \create-file \"file-name\"       -- создать пустой файл в текущей директории \n\
 \remove <folder | file>        -- удалить выборанную директорию или файл \n\
 \write-file <file> \"String\"      -- записать текст в файл \n\
 \find-file \"file-name\"         -- поиск файла в текущией директории и поддиректориях \n\
 \information <file>            -- показать информацию о файле \n\
 \information <folder>          -- показать информацию о директории \n\
 \help                          -- показать данную справку \n\
 \exit                          -- завершение работы программы"

msgDirDoesn'tExist :: String
msgDirDoesn'tExist = "dirictory douesn't exist"

msgDirAlreadyExist :: String
msgDirAlreadyExist = "directory already exists"

msgFilDoesn'tExist :: String
msgFilDoesn'tExist = "file douesn't exist"

msgFilAlreadyExist :: String
msgFilAlreadyExist = "file already exists"

msgDirOrFileDoesn'tExist :: String
msgDirOrFileDoesn'tExist = "dirictory or file douesn't exist"

msgExit :: String
msgExit = "Leaving Sheol File Manager."