(defun vc-x86-env ()
  (interactive)
  (setenv "VSINSTALLDIR" "C:\\Program Files (x86)\\Microsoft Visual Studio 8")
  (setenv "VCINSTALLDIR" "C:\\Program Files (x86)\\Microsoft Visual Studio 8\\VC")
  (setenv "FrameworkDir" "C:\\WINDOWS\\Microsoft.NET\\Framework")
  (setenv "FrameworkVersion" "v2.0.50727")
  (setenv "FrameworkSDKDir" "C:\\Program Files (x86)\\Microsoft Visual Studio 8\\SDK\\v2.0")
  (setenv "CommonDevEnvDir" "C:\\Program Files (x86)\\Microsoft Visual Studio 8\\Common7")
  (setenv "DevEnvDir" 
	  (concat (getenv "CommonDevEnvDir") "\\IDE")) ;; "C:\\Program\ Files\ (x86)\\Microsoft\ Visual\ Studio\ 8\\Common7\\IDE")
  (setenv "PATH" 
	  (concat (getenv "DevEnvDir") ;;"C:\\Program\ Files\ (x86)\\Microsoft\ Visual\ Studio\ 8\\Common7\\IDE"
		  path-separator
		   (concat (getenv "VCINSTALLDIR") "\\BIN") ;;"C:\\Program\ Files (x86)\\Microsoft\ Visual\ Studio 8\\VC\\BIN"
		   path-separator
		   (concat (getenv "CommonDevEnvDir") "\\Tools") ;;  "C:\\Program\ Files\ (x86)\\Microsoft\ Visual\ Studio\ 8\\Common7\\Tools"
		   path-separator
		   (concat (getenv "CommonDevEnvDir") "\\Tools\\bin") ;;   "C:\\Program\ Files\ (x86)\\Microsoft\ Visual\ Studio\ 8\\Common7\\Tools\\bin"
		   path-separator
		   (concat (getenv "VCINSTALLDIR") "\\PlatformSDK\\bin") ;;  "C:\\Program\ Files\ (x86)\\Microsoft\ Visual\ Studio\ 8\\VC\\PlatformSDK\\bin"
		   path-separator
		   (concat (getenv "FrameworkSDKDir") "\\bin") ;; "C:\\Program\ Files\ (x86)\\Microsoft\ Visual\ Studio\ 8\\SDK\\v2.0\\bin"
		   path-separator
		   (concat (getenv "FrameworkDir") "\\" (getenv "FrameworkVersion")) ;;  "C:\\WINDOWS\\Microsoft.NET\\Framework\\v2.0.50727"
		   path-separator
		   (concat (getenv "VCINSTALLDIR") "\\VCPackages") ;;  "C:\\Program\ Files\ (x86)\\Microsoft\ Visual\ Studio\ 8\VC\VCPackages"
		   path-separator
		   (getenv "PATH")))
  (setenv "INCLUDE"
	  (concat 
	   (concat (getenv "VCINSTALLDIR") "\\ATLMFC\\INCLUDE")
	   path-separator
	   (concat (getenv "VCINSTALLDIR") "\\INCLUDE")
	   path-separator
	   (concat (getenv "VCINSTALLDIR") "\\PlatformSDK\\include")
	   path-separator
	   (concat (getenv "VSINSTALLDIR") "\\SDK\\v2.0\\include")
	   path-separator 
	   (getenv "INCLUDE")))
  (setenv "LIB" (concat 
		 (concat (getenv "VCINSTALLDIR") "\\ATLMFC\\LIB")
		 path-separator
		 (concat (getenv "VCINSTALLDIR") "\\LIB")
		 path-separator
		 (concat (getenv "VCINSTALLDIR") "\\PlatformSDK\\lib")
		 path-separator
		 (concat (getenv "VSINSTALLDIR") "\\SDK\\v2.0\lib")
		 path-separator 
		 (getenv "LIB")))
  (setenv  "LIBPATH" 
	   (concat
	    (concat (getenv "FrameworkDir") "\\" (getenv "FrameworkVersion")) ;;  "C:\\WINDOWS\\Microsoft.NET\\Framework\\v2.0.50727"
	    path-separator
	    (concat (getenv "VCINSTALLDIR") "\\ATLMFC\\LIB")))
  (shell))

  (provide 'vc-x86-env)