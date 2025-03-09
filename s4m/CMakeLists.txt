# to rerun this, go to build dir and do Cmake -G Xcode ..

include(${CMAKE_CURRENT_SOURCE_DIR}/../../max-sdk-base/script/max-pretarget.cmake)

#############################################################
# MAX EXTERNAL
#############################################################

include_directories( 
	"${MAX_SDK_INCLUDES}"
	"${MAX_SDK_MSP_INCLUDES}"
	"${MAX_SDK_JIT_INCLUDES}"
)

# all included files go here
file(GLOB PROJECT_SRC
	"s4m-helpers.h"
	"s4m-helpers.c"
	"s4m.c"
	"s7.h"
	"s7.c"
  "s4m-grid.h"
  "s4m-grid.c"
  "s4m-carray.h"
  "s4m-msp.h"
  "s4m-msp.c"
)

# this specifies output
add_library( 
	${PROJECT_NAME} 
	MODULE
	${PROJECT_SRC}
)

include(${CMAKE_CURRENT_SOURCE_DIR}/../../max-sdk-base/script/max-posttarget.cmake)
