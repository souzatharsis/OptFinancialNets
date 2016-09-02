include_directories(${OPTFINANCIALNETS_SOURCE_DIR}/src)
link_directories(${OPTFINANCIALNETS_BINARY_DIR}/src)


SET (Boost_USE_STATIC_LIBS    ON)
SET (Boost_USE_STATIC_RUNTIME ON)
find_package(Boost COMPONENTS regex REQUIRED)

#find_library(SQLITE_LIBRARY_RELEASE sqlite3 VARIANT static)

find_package(CPLEX)
if (CPLEX_FOUND)
else ()
endif ()



add_executable(${OPTFINANCIALNETS_COMPILED} 
      main.cc
      Option.h                Option.cc
      Options.h               Options.cc
      Solver.h                Solver.cc
      CPLEX.h                 CPLEX.cc
      Model.h                 Model.cc
      AssortMST.h             AssortMST.cc
      Data.h                  Data.cc
      Util.h                  Util.cc)

target_link_libraries(${OPTFINANCIALNETS_COMPILED} m)
target_link_libraries(${OPTFINANCIALNETS_COMPILED} ${SQLITE_LIBRARY_RELEASE})
#target_link_libraries(${OPTFINANCIALNETS_COMPILED} sqlite3)
target_link_libraries(${OPTFINANCIALNETS_COMPILED} pthread)
#target_link_libraries(${OPTFINANCIALNETS_COMPILED} boost_regex)
target_link_libraries(${OPTFINANCIALNETS_COMPILED} ${Boost_LIBRARIES})
target_link_libraries(${OPTFINANCIALNETS_COMPILED} cplex-library)
target_link_libraries(${OPTFINANCIALNETS_COMPILED} cplex-concert)
target_link_libraries(${OPTFINANCIALNETS_COMPILED} ilocplex)
