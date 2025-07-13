! d7997ccd-145b-4ca1-9d13-aa166affe43f
!
! #!/bin/bash
! lynx -dump https://fpm.fortran-lang.org/spec/manifest.html >manifest.txt
! vi manifest.txt
!
program fpm_manifest
character(len=:),allocatable :: license_text(:) 
integer                      :: i 
   call manifest_txt()
   write(*,'(a)') (trim(license_text(i)), i=1, size(license_text))
contains
subroutine manifest_txt ()
license_text=[ CHARACTER(LEN=105) :: &
'123456789-123456789-123456789-123456789-123456789-123456789-123456789-123456789-',&
'   #Manifest reference                                                          ',&
'   #                                                                            ',&
'   #   The fpm.toml file for each project is called its manifest. It is         ',&
'   #   written using the TOML format. Every manifest file consists of the       ',&
'   #   following sections:                                                      ',&
'   #     * name: The name of the project                                        ',&
'   #     * version: The version of the project                                  ',&
'   #     * license: The project license                                         ',&
'   #     * maintainer: Maintainer of the project                                ',&
'   #     * author: Author of the project                                        ',&
'   #     * copyright: Copyright of the project                                  ',&
'   #     * description: Description of the project                              ',&
'   #     * categories: Categories associated with the project                   ',&
'   #     * keywords: Keywords describing the project                            ',&
"   #     * homepage: The project's homepage                                        ",&
'   #     * Build configuration:                                                 ',&
'   #          + auto-tests: Toggle automatic discovery of test                  ',&
'   #            executables                                                     ',&
'   #          + auto-examples: Toggle automatic discovery of example            ',&
'   #            programs                                                        ',&
'   #          + auto-executables: Toggle automatic discovery of                 ',&
'   #            executables                                                     ',&
'   #          + link: Link with external dependencies                           ',&
'   #          + external-modules: Specify modules used that are not             ',&
'   #            within your fpm package                                         ',&
'   #     * Fortran configuration:                                               ',&
'   #          + implicit-typing: Toggle default implicit typing                 ',&
'   #          + implicit-external: Toggle implicit external interfaces          ',&
'   #          + source-form: Select source form for project                     ',&
'   #     * Target sections:                                                     ',&
'   #          + library Configuration of the library target                     ',&
'   #          + executable Configuration of the executable targets              ',&
'   #          + test Configuration of the test targets                          ',&
'   #     * Dependency sections:                                                 ',&
'   #          + dependencies: Project library dependencies                      ',&
'   #          + dev-dependencies: Dependencies only needed for tests            ',&
'   #     * install: Installation configuration                                  ',&
'   #     * preprocess Preprocessor configuration                                ',&
'   #     * extra: Additional free data field                                    ',&
'   #                                                                            ',&
'   #Project name                                                                ',&
'   #                                                                            ',&
'   #   The project name identifies the package and is used to refer to it. It   ',&
'   #   is used when listing the project as dependency for another package and   ',&
'   #   the default name of the library and executable target. Therefore, the    ',&
'   #   project name must always be present.                                     ',&
'   #                                                                            ',&
'name = "hello_world"                                                         ',&
'   #                                                                            ',&
'   #Project version                                                             ',&
'   #                                                                            ',&
'   #   The version number of the project is specified as string. A              ',&
'   #   standardized way to manage and specify versions is the Semantic          ',&
'   #   Versioning scheme.                                                       ',&
'   #                                                                            ',&
'version = "0.1.0"                                                               ',&
'   #                                                                            ',&
'   #   The version entry can also contain a filename relative to the project    ',&
'   #   root, which contains the version number of the project                   ',&
'   #                                                                            ',&
'#version = "VERSION"                                                            ',&
'   #                                                                            ',&
'   #Project license                                                             ',&
'   #                                                                            ',&
'   #   The project license field contains the license identifier. A             ',&
'   #   standardized way to specify licensing information are SPDX               ',&
'   #   identifiers.                                                             ',&
'   #                                                                            ',&
'   #   Examples:                                                                ',&
'   #                                                                            ',&
'   #   Projects licensed under the GNU Lesser General Public License,           ',&
'   #   either version 3 or any later version, is specified as                   ',&
'   #                                                                            ',&
'   #      license = "LGPL-3.0-or-later"                                         ',&
'   #                                                                            ',&
'   #   Dual licensed project under the Apache license, version 2.0 or the       ',&
'   #   MIT license are specified as                                             ',&
'   #                                                                            ',&
'   #      license = "Apache-2.0 OR MIT"                                         ',&
'license = "license"                                                          ',&
'   #                                                                            ',&
'   #Project maintainer                                                          ',&
'   #                                                                            ',&
'   #   Information on the project maintainer and means to reach out to them.    ',&
'   #                                                                            ',&
'maintainer = "jane.doe@example.com"                                          ',&
'   #                                                                            ',&
'   #Project author                                                              ',&
'   #                                                                            ',&
'   #   Information on the project author.                                       ',&
'   #                                                                            ',&
'author = "Jane Doe"                                                          ',&
'   #                                                                            ',&
'   #Project copyright                                                           ',&
'   #                                                                            ',&
'   #   A statement clarifying the copyright status of the project.              ',&
'   #                                                                            ',&
'copyright = "Copyright 2020 Jane Doe"                                           ',&
'   #                                                                            ',&
'   #Project description                                                         ',&
'   #                                                                            ',&
'   #   The description provides a short summary on the project. It should be    ',&
'   #   plain text and not using any markup formatting.                          ',&
'   #                                                                            ',&
'description = "A short summary on this project"                              ',&
'   #                                                                            ',&
'   #Project categories                                                          ',&
'   #                                                                            ',&
'   #   The project can be associated with different categories.                 ',&
'   #                                                                            ',&
'categories = ["io"]                                                          ',&
'   #                                                                            ',&
'   #Project keywords                                                            ',&
'   #                                                                            ',&
'   #   The keywords field is an array of strings describing the project.        ',&
'   #                                                                            ',&
'keywords = ["hdf5", "mpi"]                                                      ',&
'   #                                                                            ',&
'   #Project homepage                                                            ',&
'   #                                                                            ',&
'   #   URL to the webpage of the project.                                       ',&
'   #                                                                            ',&
'homepage = "https://stdlib.fortran-lang.org"                       ',&
'   #                                                                            ',&
'   #Project targets                                                             ',&
'   #                                                                            ',&
'   #   Every fpm project can define library, executable and test targets.       ',&
'   #   Library targets are exported and useable for other projects.             ',&
'   #                                                                            ',&
'   #Library configuration                                                       ',&
'   #                                                                            ',&
'   #   Defines the exported library target of the project. A library is         ',&
'   #   generated if the source directory or include directory is found in a     ',&
'   #   project. The default source and include directories are src and          ',&
'   #   include; these can be modified in the library section using the          ',&
'   #   source-dir and include-dir entries. Paths for the source and include     ',&
'   #   directories are given relative to the project root and use / as path     ',&
'   #   separator on all platforms.                                              ',&
'   #                                                                            ',&
'[library]                                                          ',&
'source-dir = "lib"                                                 ',&
'include-dir = "inc"                                                ',&
'   #                                                                            ',&
'   #Include directory                                                           ',&
'   #                                                                            ',&
'   #   Projects which use the Fortran include statement or C preprocessor       ',&
'   #   #include statement, can use the include-dir key to specify search        ',&
'   #   directories for the included files. include-dir can contain one or more  ',&
'   #   directories, where multiple directories are specified using a list of    ',&
'   #   strings. Include directories from all project dependencies are passed    ',&
'   #   to the compiler using the appropriate compiler flag.                     ',&
'   #                                                                            ',&
'   #   Example:                                                                 ',&
'   #                                                                            ',&
'   #         [library]                                                          ',&
'   #         include-dir = ["include", "third_party/include"]                   ',&
'   #                                                                            ',&
'   #   Note                                                                     ',&
'   #                                                                            ',&
'   #   include-dir does not currently allow using pre-built module .mod files   ',&
'   #                                                                            ',&
'   #Executable targets                                                          ',&
'   #                                                                            ',&
'   #   Executable targets are Fortran programs defined as executable sections.  ',&
'   #   If no executable section is specified the app directory is searched for  ',&
'   #   program definitions. For explicitly specified executables the name       ',&
'   #   entry must always be specified. The source directory for each            ',&
'   #   executable can be adjusted in the source-dir entry. Paths for the        ',&
'   #   source directory are given relative to the project root and use / as     ',&
'   #   path separator on all platforms. The source file containing the program  ',&
'   #   body can be specified in the main entry.                                 ',&
'   #                                                                            ',&
'   #   Executables can have their own dependencies. See specifying              ',&
'   #   dependencies for more details.                                           ',&
'   #                                                                            ',&
'   #   Executables can also specify their own external library dependencies.    ',&
'   #   See external libraries for more details.                                 ',&
'   #                                                                            ',&
'   #   Example:                                                                 ',&
'   #                                                                            ',&
'   #    [[executable]]                                                          ',&
'   #       name = "app-name"                                                    ',&
'   #       source-dir = "prog"                                                  ',&
'   #       main = "program.f90"                                                 ',&
'   #                                                                            ',&
'   #    [[executable]]                                                          ',&
'   #       name = "app-tool"                                                    ',&
'   #       link = "z"                                                           ',&
'   #       [executable.dependencies]                                            ',&
'   #       helloff = { git="https://gitlab.com/everythingfunctional/helloff.git" } ',&
'   #                                                                            ',&
'   #   Specifying many separate executables can be done by using inline tables  ',&
'   #   for brevity instead                                                      ',&
'   #                                                                            ',&
'   #      executable = [                                                        ',&
'   #        { name = "a-prog" },                                                ',&
'   #        { name = "app-tool", source-dir = "tool" },                         ',&
'   #      ]                                                                     ',&
'   #                                                                            ',&
'   #Example targets                                                             ',&
'   #                                                                            ',&
'   #   Example applications for a project are defined as example sections. If   ',&
'   #   no example section is specified the example directory is searched for    ',&
'   #   program definitions. For explicitly specified examples the name entry    ',&
'   #   must always be specified. The source directory for each example can be   ',&
'   #   adjusted in the source-dir entry. Paths for the source directory are     ',&
'   #   given relative to the project root and use / as path separator on all    ',&
'   #   platforms. The source file containing the program body can be specified  ',&
'   #   in the main entry.                                                       ',&
'   #                                                                            ',&
'   #   Examples can have their own dependencies. See specifying                 ',&
'   #   dependencies for more details.                                           ',&
'   #                                                                            ',&
'   #   Examples can also specify their own external library dependencies. See   ',&
'   #   external libraries for more details.                                     ',&
'   #                                                                            ',&
'   #   Example:                                                                 ',&
'   #                                                                            ',&
'   #      [[example]]                                                           ',&
'   #      name = "demo-app"                                                     ',&
'   #      source-dir = "demo"                                                   ',&
'   #      main = "program.f90"                                                  ',&
'   #                                                                            ',&
'   #      [[example]]                                                           ',&
'   #      name = "example-tool"                                                 ',&
'   #      link = "z"                                                            ',&
'   #      [example.dependencies]                                                ',&
'   #      helloff = { git = "https://gitlab.com/everythingfunctional/helloff.git" }',&
'   #                                                                            ',&
'   #Test targets                                                                ',&
'   #                                                                            ',&
'   #   Test targets are Fortran programs defined as test sections. They follow  ',&
'   #   similar rules as the executable targets. If no test section is           ',&
'   #   specified the test directory is searched for program definitions. For    ',&
'   #   explicitly specified tests the name entry must always be specified. The  ',&
'   #   source directory for each test can be adjusted in the source-dir entry.  ',&
'   #   Paths for the source directory are given relative to the project root    ',&
'   #   and use / as path separator on all platforms. The source file            ',&
'   #   containing the program body can be specified in the main entry.          ',&
'   #                                                                            ',&
'   #   Tests can have their own dependencies. See specifying dependencies       ',&
'   #   for more details.                                                        ',&
'   #                                                                            ',&
'   #   Tests can also specify their own external library dependencies. See      ',&
'   #   external libraries for more details.                                     ',&
'   #                                                                            ',&
'   #   Example:                                                                 ',&
'   #                                                                            ',&
'   #    [[test]]                                                                ',&
'   #    name = "test-name"                                                      ',&
'   #    source-dir = "testing"                                                  ',&
'   #    main = "tester.F90"                                                     ',&
'   #                                                                            ',&
'   #    [[test]]                                                                ',&
'   #    name = "tester"                                                         ',&
'   #    link = ["blas", "lapack"]                                               ',&
'   #    [test.dependencies]                                                     ',&
'   #    helloff = { git="https://gitlab.com/everythingfunctional/helloff.git"}  ',&
'   #                                                                            ',&
'   #Link external libraries                                                     ',&
'   #                                                                            ',&
'   #   To declare link time dependencies on external libraries a list of        ',&
'   #   native libraries can be specified in the link entry. Specify either one  ',&
'   #   library as string or a list of strings in case several libraries should  ',&
'   #   be linked. When possible the project should only link one native         ',&
'   #   library. The list of library dependencies is exported to dependent       ',&
'   #   packages.                                                                ',&
'   #                                                                            ',&
'   #   Example:                                                                 ',&
'   #                                                                            ',&
'   #   To link against the zlib compression library use                         ',&
'   #                                                                            ',&
'   #      [build]                                                               ',&
'   #      link = "z"                                                            ',&
'   #                                                                            ',&
'   #   To dependent on LAPACK also BLAS should be linked. In this case the      ',&
'   #   order of the libraries matters:                                          ',&
'   #                                                                            ',&
'   #      [build]                                                               ',&
'   #      link = ["blas", "lapack"]                                             ',&
'   #                                                                            ',&
'   #Use system-installed modules                                                ',&
'   #                                                                            ',&
'   #   To use modules that are not defined within your fpm package or its       ',&
'   #   dependencies, specify the module name using the external-modules key in  ',&
'   #   the build table.                                                         ',&
'   #                                                                            ',&
'   #   Important                                                                ',&
'   #                                                                            ',&
'   #   fpm cannot automatically locate external module files; it is the         ',&
'   #   responsibility of the user to specify the necessary include directories  ',&
'   #   using compiler flags such that the compiler can locate external module   ',&
'   #   files during compilation.                                                ',&
'   #                                                                            ',&
'   #   Example:                                                                 ',&
'   #                                                                            ',&
'   #      [build]                                                               ',&
'   #      external-modules = "netcdf"                                           ',&
'   #                                                                            ',&
'   #   Multiple external modules can be specified as a list.                    ',&
'   #                                                                            ',&
'   #   Example:                                                                 ',&
'   #                                                                            ',&
'   #      [build]                                                               ',&
'   #      external-modules = ["netcdf", "h5lt"]                                 ',&
'   #                                                                            ',&
'   #Automatic target discovery                                                  ',&
'   #                                                                            ',&
'   #   Executables and test can be discovered automatically in their default    ',&
'   #   directories. The automatic discovery recursively searches the app,       ',&
'   #   example, and test directories for program definitions and declares them  ',&
'   #   as executable, example, and test targets, respectively. The automatic    ',&
'   #   discovery is enabled by default.                                         ',&
'   #                                                                            ',&
'   #   To disable the automatic discovery of targets set the auto-executables,  ',&
'   #   auto-examples, and auto-tests entry to false.                            ',&
'   #                                                                            ',&
'   #      [build]                                                               ',&
'   #      auto-executables = false                                              ',&
'   #      auto-examples = false                                                 ',&
'   #      auto-tests = false                                                    ',&
'   #                                                                            ',&
'   #Fortran features                                                            ',&
'   #                                                                            ',&
'   #   Allows to enable and disable specific language features                  ',&
'   #                                                                            ',&
'   #Implicit typing                                                             ',&
'   #                                                                            ',&
'   #   Allows to toggle whether the default implicit typing should be used.     ',&
'   #   The default option is false.                                             ',&
'   #                                                                            ',&
'   #      [fortran]                                                             ',&
'   #      implicit-typing = true  # default: false                              ',&
'   #                                                                            ',&
'   #Implicit external                                                           ',&
'   #                                                                            ',&
'   #   Allows to toggle whether external interfaces can be declared             ',&
'   #   implicitly. The default option is false.                                 ',&
'   #                                                                            ',&
'   #      [fortran]                                                             ',&
'   #      implicit-external = true  # default: false                            ',&
'   #                                                                            ',&
'   #Source form                                                                 ',&
'   #                                                                            ',&
'   #   Allows to specifiy the source form to be used for all files in the       ',&
'   #   project. Possible choices are "free" to assume all files are free form   ',&
'   #   source, "fixed" to assume all files are fixed form source, and           ',&
'   #   "default" to let the compiler decide based on its own heuristics. The    ',&
'   #   default option is "free".                                                ',&
'   #                                                                            ',&
'   #      [fortran]                                                             ',&
'   #      source-form = "fixed"  # default: "free"                              ',&
'   #                                                                            ',&
'   #Specifying dependencies                                                     ',&
'   #                                                                            ',&
'   #   Dependencies can be declared in the dependencies table in the manifest   ',&
'   #   root or the executable or test sections. When declared in the            ',&
'   #   manifest root the dependencies are exported with the project.            ',&
'   #                                                                            ',&
'   #Dependencies from version control systems                                   ',&
'   #                                                                            ',&
'   #   Dependencies can be specified by the projects git repository.            ',&
'   #                                                                            ',&
'   #      [dependencies]                                                        ',&
'   #      toml-f = { git = "https://github.com/toml-f/toml-f" }                 ',&
'   #                                                                            ',&
'   #   To use a specific upstream branch declare the branch name with           ',&
'   #                                                                            ',&
'   #      [dependencies]                                                        ',&
'   #      toml-f = { git = "https://github.com/toml-f/toml-f", branch = "main" }',&
'   #                                                                            ',&
'   #   Alternatively, reference tags by using the tag entry                     ',&
'   #                                                                            ',&
'   #      [dependencies]                                                        ',&
'   #      toml-f = { git = "https://github.com/toml-f/toml-f", tag = "v0.2.1" } ',&
'   #                                                                            ',&
'   #   To pin a specific revision specify the commit hash in the rev entry      ',&
'   #                                                                            ',&
'   #      [dependencies]                                                        ',&
'   #      toml-f = { git = "https://github.com/toml-f/toml-f", rev = "2f5eaba" }',&
'   #                                                                            ',&
'   #   For more verbose layout use normal tables rather than inline tables to   ',&
'   #   specify dependencies                                                     ',&
'   #                                                                            ',&
'   #      [dependencies]                                                        ',&
'   #      [dependencies.toml-f]                                                 ',&
'   #      git = "https://github.com/toml-f/toml-f"                              ',&
'   #      rev = "2f5eaba864ff630ba0c3791126a3f811b6e437f3"                      ',&
'   #                                                                            ',&
'   #Dependencies from a registry                                                ',&
'   #                                                                            ',&
'   #   Note                                                                     ',&
'   #                                                                            ',&
'   #   To enable the usage of a registry in fpm make sure you read the          ',&
'   #   instructions in the registry section first.                              ',&
'   #                                                                            ',&
'   #Namespace                                                                   ',&
'   #                                                                            ',&
'   #   Packages obtained from a registry (both remote and local) are required   ',&
'   #   to specify a namespace, which provides a way to uniquely identify and    ',&
'   #   differentiate packages with identical names. The namespace is declared   ',&
'   #   in the manifest (fpm.toml).                                              ',&
'   #                                                                            ',&
'   #      [dependencies]                                                        ',&
'   #      my-package.namespace = "my-namespace"                                 ',&
'   #                                                                            ',&
'   #   This will prompt fpm to download the newest version of "my-package",     ',&
'   #   which belongs to "my-namespace", from the registry.                      ',&
'   #                                                                            ',&
'   #Version                                                                     ',&
'   #                                                                            ',&
'   #   If you want to download a specific version of a package instead of the   ',&
'   #   newest one available, you can specify the version (v) in the manifest.   ',&
'   #                                                                            ',&
'   #      [dependencies]                                                        ',&
'   #      example-package.namespace = "example-namespace"                       ',&
'   #      example-package.v = "1.0.0"                                           ',&
'   #                                                                            ',&
'   #Local dependencies                                                          ',&
'   #                                                                            ',&
'   #   To declare local dependencies use the path entry.                        ',&
'   #                                                                            ',&
'   #      [dependencies]                                                        ',&
'   #      my-utils = { path = "utils" }                                         ',&
'   #                                                                            ',&
'   #   The local dependency path is given relative to the fpm.toml it is        ',&
'   #   written to, and uses / as the path separator on all platforms.           ',&
'   #                                                                            ',&
'   #Dependency-specific macro setting                                           ',&
'   #                                                                            ',&
'   #   As of fpm>=0.9.1, an array of dependency-specific macros can be passed   ',&
'   #   to a single dependency from the manifest, in the same fashion as in the  ',&
'   #   manifest''s preprocessor configuration table. Its preprocess table       ',&
'   #   needs to be entered as part of the dependency entry. fpm will not check  ',&
'   #   if the passed macros collide with the dependencie''s own manifest, so,   ',&
'   #   it is the user’s responsibility to ensure that no collisions or          ',&
'   #   unexpected behavior occur. For example, one can control the REAL         ',&
'   #   precision that one library is to be used with:                           ',&
'   #                                                                            ',&
'   #      [dependencies]                                                        ',&
'   #      fftpack = """                                                         ',&
'   # { git="https://github.com/fortran-lang/fftpack.git",\                      ',&
'   #       preprocess.cpp.macros = ["REAL32"] }"""                              ',&
'   #                                                                            ',&
'   #Development dependencies                                                    ',&
'   #                                                                            ',&
'   #   Development dependencies allow to declare dev-dependencies in the        ',&
'   #   manifest root, which are available to all tests but not exported with    ',&
'   #   the project.                                                             ',&
'   #                                                                            ',&
'   #Installation configuration                                                  ',&
'   #                                                                            ',&
'   #   In the install section components for the installation can be selected.  ',&
'   #   By default only executables are installed, library projects can set the  ',&
'   #   library boolean to also installatation the module files and the          ',&
'   #   archive.                                                                 ',&
'   #                                                                            ',&
'   #   Example                                                                  ',&
'   #                                                                            ',&
'   #      [install]                                                             ',&
'   #      library = true                                                        ',&
'   #                                                                            ',&
'   #Preprocessor configuration                                                  ',&
'   #                                                                            ',&
'   #   Under the preprocess section, you can specify one or more preprocessor   ',&
'   #   to use in an fpm project.                                                ',&
'   #                                                                            ',&
'   #Specifying the preprocessor                                                 ',&
'   #                                                                            ',&
'   #   The preprocess section allows one or more preprocessors to be            ',&
'   #   specified. For example, cpp can be specified like this :                 ',&
'   #                                                                            ',&
'   #   Example                                                                  ',&
'   #                                                                            ',&
'   #      [preprocess]                                                          ',&
'   #      [preprocess.cpp]                                                      ',&
'   #                                                                            ',&
'   #   To use multiple preprocessors, for example cpp and fypp, specify them    ',&
'   #   like this:                                                               ',&
'   #                                                                            ',&
'   #   Example                                                                  ',&
'   #                                                                            ',&
'   #      [preprocess]                                                          ',&
'   #      [preprocess.cpp]                                                      ',&
'   #      [preprocess.fypp]                                                     ',&
'   #                                                                            ',&
'   #   You can also specify source file suffixes that the preprocessor should   ',&
'   #   run on:                                                                  ',&
'   #                                                                            ',&
'   #   Example                                                                  ',&
'   #                                                                            ',&
'   #      [preprocess]                                                          ',&
'   #      [preprocess.cpp]                                                      ',&
'   #      suffixes = ["F90", "f90"]                                             ',&
'   #                                                                            ',&
'   #   Further, you can instruct the preprocessor to run on source files in     ',&
'   #   specific directories:                                                    ',&
'   #                                                                            ',&
'   #   Example                                                                  ',&
'   #                                                                            ',&
'   #      [preprocess]                                                          ',&
'   #      [preprocess.cpp]                                                      ',&
'   #      directories = ["src/feature1", "src/models"]                          ',&
'   #                                                                            ',&
'   #   Preprocessor macros can be defined like this:                            ',&
'   #                                                                            ',&
'   #   Example                                                                  ',&
'   #                                                                            ',&
'   #      [preprocess]                                                          ',&
'   #      [preprocess.cpp]                                                      ',&
'   #      macros = ["FOO", "BAR"]                                               ',&
'   #                                                                            ',&
'   #   We can also use dotted keys to define our preprocessor settings.         ',&
'   #                                                                            ',&
'   #   Example                                                                  ',&
'   #                                                                            ',&
'   #      [preprocess]                                                          ',&
'   #      cpp.suffixes = ["F90", "f90"]                                         ',&
'   #      cpp.directories = ["src/feature1", "src/models"]                      ',&
'   #      cpp.macros = ["FOO", "BAR"]                                           ',&
'   #                                                                            ',&
'   #   We can also define valued macros in preprocess table.                    ',&
'   #                                                                            ',&
'   #   Example                                                                  ',&
'   #                                                                            ',&
'   #      [preprocess]                                                          ',&
'   #      [preprocess.cpp]                                                      ',&
'   #      macros=["FOO=2", "BAR=4"]                                             ',&
'   #                                                                            ',&
'   #   We can also reuse values like version number from manifest as a value    ',&
'   #   for a macro.                                                             ',&
'   #                                                                            ',&
'   #   Example                                                                  ',&
'   #                                                                            ',&
'   #      version = "1"                                                         ',&
'   #                                                                            ',&
'   #      [preprocess]                                                          ',&
'   #      [preprocess.cpp]                                                      ',&
'   #      macros=["VERSION={version}"]                                          ',&
'   #                                                                            ',&
'   #Additional free data field                                                  ',&
'   #                                                                            ',&
'   #   Third-party tools can store their configuration inside the extra         ',&
'   #   section. This section will never be evaluated by fpm itself, the only    ',&
'   #   constraint imposed is that it has to be valid TOML.                      ',&
'   #                                                                            ',&
'   #   Since the format of this section is free, only recommendations are       ',&
'   #   provided here for adding data to the extra section.                      ',&
'   #    1. Only use subtables, never add configuration data to the top level    ',&
'   #       of the extra section. Reasoning: different tools can avoid           ',&
'   #       collisions of key names by placing their data in separate            ',&
'   #       subtables.                                                           ',&
'   #    2. Use the concrete name of the tool rather than a generic name for     ',&
'   #       the subtable. Reasoning: different formatter or linter tools might   ',&
'   #       use conflicting keywords in a format or lint subtable. Also, users   ',&
'   #       can tell from the table name which tool is preferred to use with     ',&
'   #       the project.                                                         ',&
'   #    3. Fpm plugins should use a subtable with their plugin name in the      ',&
'   #       extra.fpm section to store their data. Reasoning: following this     ',&
'   #       convention provides the user of fpm plugins with one section to      ',&
'   #       configure their used plugins.                                        ',&
'   #    4. Use the fpm preferred style for keywords which is lowercase with     ',&
'   #       dashes. Reasoning: while there is no style check in this section, a  ',&
'   #       consistent style in the whole manifest will make it easier for the   ',&
'   #       user to understand the whole package manifest.                       ',&
'   #                                                                            ',&
'   #   Feedback for the recommendations above is very much welcome. If you      ',&
'   #   have a tool that uses the extra section in the package manifest, feel    ',&
'   #   free to post it in at the fpm discussion board.                          ',&
'   #                                                                            ',&
'   #   By fpm contributors                                                      ',&
'   #   © Copyright 2021 Fortran programming language community.                 ',&
'   #']
end subroutine manifest_txt 
end program fpm_manifest
