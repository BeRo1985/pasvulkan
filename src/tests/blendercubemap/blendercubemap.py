# This script assigns OpenGL-style CubeMap matrices to cameras in Blender.
# The script assumes that the cameras are already created and named accordingly.
# The script also assumes that the cameras are already positioned at the origin.

import bpy
import mathutils

# OpenGL-Style CubeMap matrices (Column-Major)
CubeMapMatrices = [
    # pos x
    ((0.0, 0.0, -1.0, 0.0), (0.0, -1.0, 0.0, 0.0), (-1.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 1.0)),
    # neg x
    ((0.0, 0.0, 1.0, 0.0), (0.0, -1.0, 0.0, 0.0), (1.0, 0.0, 0.0, 0.0), (0.0, 0.0, 0.0, 1.0)),
    # pos y
    ((1.0, 0.0, 0.0, 0.0), (0.0, 0.0, -1.0, 0.0), (0.0, 1.0, 0.0, 0.0), (0.0, 0.0, 0.0, 1.0)),
    # neg y
    ((1.0, 0.0, 0.0, 0.0), (0.0, 0.0, 1.0, 0.0), (0.0, -1.0, 0.0, 0.0), (0.0, 0.0, 0.0, 1.0)),
    # pos z
    ((1.0, 0.0, 0.0, 0.0), (0.0, -1.0, 0.0, 0.0), (0.0, 0.0, -1.0, 0.0), (0.0, 0.0, 0.0, 1.0)),
    # neg z
    ((-1.0, 0.0, 0.0, 0.0), (0.0, -1.0, 0.0, 0.0), (0.0, 0.0, 1.0, 0.0), (0.0, 0.0, 0.0, 1.0)),
]

# Camera names in Blender (name your cameras accordingly)
camera_names = ["camera_pos_x", "camera_neg_x", "camera_pos_y", "camera_neg_y", "camera_pos_z", "camera_neg_z"]

# Ensure that there are exactly 6 cameras
if len(camera_names) != len(CubeMapMatrices):
    raise ValueError("The number of cameras does not match the CubeMap matrices.")

# Matrix assignment
for i, matrix in enumerate(CubeMapMatrices):
    # Get the camera
    camera = bpy.data.objects.get(camera_names[i])
    if not camera:
        raise ValueError(f"Kamera '{camera_names[i]}' nicht gefunden!")
    
    # Transpose the OpenGL matrix (Column-Major -> Row-Major for Blender)
    blender_matrix = mathutils.Matrix(matrix).transposed()

    # Apply matrix to the camera
    camera.matrix_world = blender_matrix
    print(f"Matrix for camera {camera_names[i]} applied.")
