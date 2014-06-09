cmake_minimum_required(VERSION 2.8)
project(BBLAUNCHER)
find_program(CABAL cabal PATHS $ENV{APPDATA}/cabal/bin NO_DEFAULT_PATH)
if(NOT CABAL) 
  message(FATAL_ERROR "Could not find updated cabal! Please run 'cabal install cabal-install'")
endif()
message(STATUS "Found cabal at ${CABAL}")

set(LINKBOT_LABS_EXE_NAME "LinkbotLabs")
set(LINKBOT_LABS_MAIN_FILE "${PROJECT_SOURCE_DIR}/Launcher.hs")
configure_file(Launcher.cabal.in Launcher.cabal @ONLY)

execute_process(COMMAND ${CABAL} sandbox init)
add_custom_target(bblauncher ALL ${CABAL} install ${PROJECT_BINARY_DIR})
install(PROGRAMS "${PROJECT_BINARY_DIR}/.cabal-sandbox/bin/${LINKBOT_LABS_EXE_NAME}${CMAKE_EXECUTABLE_SUFFIX}" 
        DESTINATION .)
