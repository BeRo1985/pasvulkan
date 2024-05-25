#!/bin/bash
wc -l *.pas *.inc *.dpr ../externals/kraft/src/kraft.pas \
                        ../externals/kraft/src/KraftArcadeCarPhysics.pas \
                        ../externals/kraft/src/KraftRayCastVehicle.pas \
                        ../externals/pasdblstrutils/src/PasDblStrUtils.pas \
                        ../externals/pasgltf/src/PasGLTF.pas \
                        ../externals/pasjson/src/PasJSON.pas \
                        ../externals/pasmp/src/PasMP.pas \
                        ../externals/pucu/src/PUCU.pas \
                        ../externals/poca/src/POCA.pas \
                        ../externals/rnl/src/RNL.pas \
                        ./assets/shaders/canvas/*.glsl \
                        ./assets/shaders/canvas/*.frag \
                        ./assets/shaders/canvas/*.vert \
                        ./assets/shaders/scene3d/*.glsl \
                        ./assets/shaders/scene3d/*.frag \
                        ./assets/shaders/scene3d/*.vert \
                        ./assets/shaders/scene3d/*.geom \
                        ./assets/shaders/scene3d/*.tesc \
                        ./assets/shaders/scene3d/*.tese \
                        ./assets/shaders/scene3d/*.comp \
                        ./assets/shaders/scene3d/*.cpp \
                        ./assets/shaders/scene3d/*.c \
                        ./assets/shaders/scene3d/*.poca
