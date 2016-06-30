# Copyright (C) 2009 The Android Open Source Project
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

OSL_HOME_FLAGS = -DOSL_HOME=\"../../osl\"

LOCAL_MODULE    := gpsfish-$(TARGET_ARCH_ABI)
LOCAL_SRC_FILES := ../src/mainone.cc
LOCAL_CXXFLAGS  := -std=c++11 -DOSL_SMP -O3 -DNDEBUG -DGPSFISH -DPROMOTE_AS_CAPTURE -DGPSFISH_HOME=\"./data\" -DNO_PREFETCH $(OSL_HOME_FLAGS) -DOSL_NO_SSE
LOCAL_CXXFLAGS += -I$(NDK_ROOT)/boost_1_53_0/include -I$(NDK_ROOT)/bz2/include
LOCAL_CXXFLAGS += -fPIE
LOCAL_LDFLAGS += -fPIE -pie 

LOCAL_C_INCLUDES := ../ ../../gpsshogi/include ../../gpsshogi/../osl/full ../../gpsshogi/../osl/std ../../gpsshogi/../osl/core
LOCAL_CPP_FEATURES += exceptions rtti

#LOCAL_STATIC_LIBRARIES    := -lpthread
include $(BUILD_EXECUTABLE)
