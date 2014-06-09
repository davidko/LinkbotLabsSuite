cmake_minimum_required(VERSION 2.8)
project(BBLAUNCHER)

if(CMAKE_HOST_WIN32)
    set(CABAL_BIN_DIR $ENV{APPDATA}/cabal/bin)
else()
    set(CABAL_BIN_DIR $ENV{HOME}/.cabal/bin)
endif()
find_program(CABAL cabal PATHS ${CABAL_BIN_DIR} NO_DEFAULT_PATH)
if(NOT CABAL) 
  message(FATAL_ERROR "Could not find updated cabal! Please run 'cabal install cabal-install'")
endif()
message(STATUS "Found cabal at ${CABAL}")

set(LINKBOT_LABS_EXE_NAME "LinkbotLabs")
set(LINKBOT_LABS_MAIN_FILE "${PROJECT_SOURCE_DIR}/launcher.hs")
configure_file(Launcher.cabal.in Launcher.cabal @ONLY)

execute_process(COMMAND ${CABAL} sandbox init)
add_custom_target(bblauncher ALL ${CABAL} install -j ${PROJECT_BINARY_DIR})
install(PROGRAMS "${PROJECT_BINARY_DIR}/.cabal-sandbox/bin/${LINKBOT_LABS_EXE_NAME}${CMAKE_EXECUTABLE_SUFFIX}" 
        DESTINATION .)
