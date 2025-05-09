# Renderer Design overview

## Introduction

This document provides an overview of the renderer design, focusing on the architecture and challenges associated with modern GPU-driven rendering techniques. It highlights the differences between traditional rendering methods and the current approach, particularly in terms of animation handling and memory management.

PasVulkan's renderer architecture is designed to be flexible and efficient, allowing for high-performance rendering in real-time applications. The architecture is built around the Vulkan API, which provides low-level access to the GPU and enables advanced rendering techniques.

The renderer is a strict GPU-driven architecture, meaning that all rendering operations are performed on the GPU rather than the CPU. This approach allows for better performance and scalability, as the GPU can handle large amounts of data in parallel. The renderer is a strict forward+ renderer, meaning that it uses a forward rendering pipeline with additional optimizations for handling large scenes and complex lighting.

## Why no deferred renderer? And why forward+ clustered renderer?

Deferred rendering was once a popular choice for real-time graphics, particularly in the context of complex lighting and detailed scene management. However, as GPU architectures and rendering techniques have evolved, the limitations of deferred rendering have become more apparent, leading to a shift towards forward+ rendering and other modern techniques. 

In short, while deferred rendering was a promising technique when introduced, it has proven less effective for today's GPU architecture and rendering techniques. Originally designed to enable more complex lighting models and scene details than traditional forward rendering, deferred rendering offered significant benefits in its time. However, as GPU capabilities have dramatically advanced, the demands of modern rendering, characterized by highly 
detailed scenes, diverse material types, and intricate lighting, have started to reveal practical limitations of deferred rendering. Because the greater the material variety, the more complex the lighting becomes, deferred rendering's limitations in handling multiple material types and lights simultaneously become apparent, with the G-Buffer needing to store enormous amounts of data for each pixel (e.g., normals, depth, albedo, and additional material parameters), leading to increased memory usage and bandwidth demands. This results in significantly increased memory usage and bandwidth demands, even on modern GPUs optimized for handling large amounts of data in parallel.

Furthermore, the complexity inherent to deferred rendering can also introduce additional overhead in terms of shader management and resource binding, making it less suitable for real-time applications where performance is paramount. For instance, deferred rendering relies heavily on multiple render targets (MRT) and intermediate buffers, and the necessity to store intermediate data for each pixel, which consequently stresses memory bandwidth and requires substantial storage for these buffers, challenges that older hardware could somewhat manage but become limiting under current performance expectations. Modern GPUs, despite their increased memory capacities, benefit more from methods that offer greater flexibility and efficiency in handling the vast amounts of data present in contemporary scenes.

Deferred rendering's reliance on these intermediate steps not only leads to increased memory usage and bandwidth demands but also results in additional overhead due to the complexity of shader management and resource binding. This complexity is less of an issue with forward rendering, which typically avoids the need for multiple GPU passes and the storage of extensive intermediate data, resulting in more efficient resource usage and performance. As a consequence, while deferred rendering had historical significance in enabling advanced lighting and scene detail, its limitations in adaptability and efficiency render it less suitable for modern rendering pipelines that demand high performance and flexibility.

Modern rendering techniques such as forward+ emerged to address these challenges by blending the simplicity of forward rendering with advanced tile‑based or clustered light management and culling. Forward+ divides the scene into tiles or clusters and tracks which lights affect each region. During rendering, the engine ignores lights that do not influence a given tile, reducing the number of lights processed and lowering the computational cost. This method excels in scenes with many light sources because the renderer focuses only on those that matter. For forward+, a compute shader sorts the lights into a tile or cluster structure, allowing the renderer to efficiently access only the relevant lights for each pixel. This approach minimizes the overhead associated with managing multiple render targets and intermediate buffers, which are common in deferred rendering. And Material properties live in a bindless data structure so shaders fetch them on demand without multiple render targets or large intermediate buffers, even in a random access manner without the need for a pre-defined order. This allows for a more flexible and efficient rendering pipeline, as shaders can access only the data they need without the overhead of older methods By cutting bulky intermediate data this approach reduces memory use and bandwidth requirements compared with deferred rendering while still supporting diverse materials and lighting conditions at high performance.

Graphics hardware has also evolved to include features such as hardware‑accelerated ray tracing and advanced shading models that fit naturally into a forward+ pipeline. These capabilities enable realistic reflections, shadows and material interactions without the massive intermediate data storage that deferred rendering demands, making forward+ an ideal foundation for real‑time graphics development.

In summary, while deferred rendering once heralded a new era of complex lighting and detailed scene management, today's GPU architectures and rendering techniques have shifted towards approaches that better align with the demands of highly detailed, multi-material, and intensely lit scenes. The increased memory footprint, bandwidth usage, and overhead related to shader and resource management associated with deferred rendering have made it a less practical and viable choice for modern real-time applications, reinforcing the industry's move toward more efficient, flexible, and adaptable rendering strategies.

## Single-Buffer Renderer Architecture

At PasVulkan a single‑buffer renderer architecture is adopted to leverage modern GPU capabilities and optimize rendering performance. All model data, including vertex attributes, indices and material properties, is stored in a single buffer per data type. This approach enables efficient random memory access and removes the overhead of multiple buffer bindings during rendering, avoiding costly state changes. By using one buffer the renderer can quickly access the necessary data for each draw call without extra bindings or state changes, improving performance and reducing CPU overhead. And in the optimal case, a single draw call can render multiple meshes, provided shaders have random access to all material data and textures. This is achieved through bindless data structures and bindless textures, which allow shaders to fetch material properties on demand without the need for multiple render targets or large intermediate buffers. This approach reduces memory usage and bandwidth requirements compared to deferred rendering while still supporting diverse materials and lighting conditions at high performance.

For hardware raytracing this design is required anyway since BLAS (Bottom‑Level Acceleration Structures) must be built from the same vertex data used for rasterization. Storing that data in one buffer ensures a consistent format and layout for both rasterization and raytracing. If separate buffers were used, memory usage would increase, and data management would become more complex.

### Challenges of Modern GPU-Driven Renderer Architectures Compared to Traditional Approaches

In a GPU-driven renderer architecture, where all vertices are preprocessed per frame by a compute shader at the beginning of a frame and stored in a single "everything-in-one-single-buffer," the following problems arise regarding per-instance animations:

1. Preprocessing of Vertex Data per Frame
The compute shader calculates all vertex data for a specific animation state at the start of each frame and stores it in one large shared buffer. This data remains static and unchanged for the entire frame.

2. Static Vertex Data for All Instances
All subsequent rendering stages, including rasterization, further render passes, and hardware raytracing, access the same preprocessed vertex buffer. Consequently, all instances of the same model share the same animation state within a frame. Different animation states per instance are not possible unless separate mesh copies are created in the buffer for additional animation states, which in turn increases vRAM usage.

3. Reduction of Draw Calls Through the "Everything-in-One-Single-Buffer" Approach
The "everything-in-one-single-buffer" approach aims to reduce the number of draw calls using bindless data structures. Ideally, a single draw call can render multiple meshes, provided shaders have random access to all material data and textures. Older architectures required separate draw calls for each material and texture group, significantly increasing the number of draw calls and potentially impairing performance.

4. Support for Hi-Z Two-Pass Occlusion Culling
The "everything-in-one-single-buffer" approach is a prerequisite for Hi-Z two-pass occlusion culling. The culling compute shader needs access to all objects through a single linear object list with offset references to the respective vertex and index ranges within the large buffer. This enables efficient occlusion culling by quickly determining which objects are visible and which are not. Without a unified buffer, such efficient access would be difficult to achieve.

5. Limitations Imposed by Hardware Raytracing (BLAS)
Hardware raytracing requires the creation of Bottom-Level Acceleration Structures (BLAS), which rely on static vertex data. Although each BLAS can have its own transformation matrix, this only allows positioning, rotation, or scaling of the entire model, not changes in animation poses. Different animation states would require separate vertex data, which cannot be implemented within a single buffer.

6. Necessity for Multiple Vertex Data Sets for Different Animation States
To simultaneously represent different animation states, separate areas within the large buffer must be reserved for each required animation variant. Each "unique instance" with its own animation state thus requires individual preprocessed vertex data. This leads to a significant increase in vRAM usage because the same vertex data must be stored multiple times—for each distinct animation.

7. Simplified Calculation of Motion Vectors
Using a double-buffered "everything-in-one-single-buffer" approach simplifies the calculation of motion vectors. Direct access to the positions from the last and current frames in the shared buffer allows efficient computation of position differences for the motion vector render target. These motion vectors are essential for subsequent post-processing passes like antialiasing and motion blur, as they determine pixel motion direction and speed.

8. Comparison with Older Rendering Methods
In older GPU pipelines, animations and transformations were dynamically recalculated at runtime per render pass and pipeline stage in the vertex shader. Each instance could have its own animation state without additional memory for vertex data. However, this older method led to significant overhead in modern games because each render pass and pipeline stage continuously recalculated animations. Although the current approach with a precomputed large buffer sacrifices flexibility regarding animations per GPU instance—as all instances must use the same set of static vertices—it is often more performant overall.

### Summary

In an architecture where a compute shader precomputes and stores all vertex data per frame in a large, static buffer, the following challenges arise:

- Limited animation variety per frame: All instances of the same model share the same animation state within a frame.

- Increased memory demand for different animations: Different animation states require separate vertex data sets in memory, increasing vRAM usage.

- Reduced number of draw calls: While the "everything-in-one-single-buffer" approach reduces draw calls using bindless data structures, it complicates simultaneous representation of different animation states.

- Necessity of a unified buffer for occlusion culling: The approach supports efficient occlusion culling but limits animation flexibility per instance.

- Simplified calculation of motion vectors: The double-buffered approach simplifies motion vector calculations but further restricts animation flexibility due to consistent frame data storage.

- Limited use of hardware raytracing: Static BLAS structures prevent flexible animation changes without additional memory overhead.

Compared to older rendering methods that allowed dynamic animations per instance, the new approach is less flexible and consumes more memory when representing various animation states simultaneously. Each unique instance can only display one animation state per frame unless memory usage is increased by duplicating vertex data for each animation.

## Hi-Z Two-Pass Occlusion Culling

### Introduction

Occlusion culling is a vital and critical optimization technique in real-time 3D rendering, particularly in complex scenes with many objects. Its primary goal is to avoid rendering objects that are completely hidden (occluded) from the viewer's perspective by other objects closer to the camera. By determining which objects are visible and culling those that are not, occlusion culling helps prevent the GPU from wasting resources on vertex processing, rasterization, and pixel shading for hidden geometry. This technique is especially important in modern game engines, where performance and efficiency are critical for delivering high-quality, real-time experiences.

**Hierarchical Z-Buffer (Hi-Z) Two-Pass Occlusion Culling** is a popular and efficient GPU-driven technique to achieve this. It leverages a mipmapped representation of the depth buffer (the Hierarchical Z-Buffer) to perform occlusion tests rapidly on the GPU, minimizing CPU involvement and avoiding costly read-backs from the GPU to the CPU.

This document outlines the Hi-Z Two-Pass Occlusion Culling technique, its implementation, and its advantages in modern rendering pipelines. It also discusses the challenges associated with implementing this technique in a GPU-driven architecture, particularly in the context of the "everything-in-one-single-buffer" approach.

### Core Concept: The Hierarchical Z-Buffer (Hi-Z)

The foundation of this technique is the **Hierarchical Z-Buffer (Hi-Z)**. It's essentially a mipmap pyramid generated from the scene's depth buffer (Z-buffer).

1.  **Base Level (Mip 0):** This is the standard, full-resolution depth buffer generated by rendering some geometry (see Pass 1).
2.  **Subsequent Levels (Mip 1, Mip 2, ...):** Each subsequent level is a downsampled version of the previous one. For instance, Mip 1 might be half the width and height of Mip 0.
3.  **Depth Value Storage:** Crucially, each texel (pixel) in a higher mip level (lower resolution) stores a single depth value that represents the depth information of the corresponding block of texels (e.g., 2x2, 4x4) in the lower mip level (higher resolution). For occlusion culling, each texel in the Hi-Z typically stores the **minimum** depth value (i.e., the *nearest* Z value) within its corresponding region in the finer level.

This hierarchical structure allows for quick depth comparisons against large screen-space regions by sampling from the appropriate (coarser) mip level.

### The Two-Pass Process

As the name suggests, the technique generally operates in two - but actually three since the Z-prepass is a separate pass, but it is often combined with the Hi-Z generation pass, so it is not counted as a separate pass - main passes executed on the GPU:

#### Pass 1: Depth Pre-Pass and Hi-Z Generation

1.  **Z-Prepass (with Temporal Coherence Visibility):** 
    Render the last frame visible objects in the scene to establish an initial depth buffer, or everything (but frustum culled) into the depth buffer *only* (Z-prepass) to establish a baseline depth buffer for the Hi-Z generation. This is often done using a simplified shader that only writes depth information. The Z-prepass can be thought of as a "depth-only" rendering pass where the goal is to write the depth values of the major occluders in the scene. This pass typically renders all *static* objects and *dynamic* objects that are not occluded by other objects. The Z-prepass is crucial for establishing a baseline depth buffer for the Hi-Z generation. Important that no reprojection from the previous frame depth buffer - as it was done in the past by other implementations - is done here, since it is and was a bad idea, since it can lead to incorrect depth values and artifacts in the Hi-Z buffer. The Z-prepass should be done with the current frame's geometry to ensure that the depth information is accurate and up-to-date. So rendering only the objects that were determined to be *visible* in the last frame into the current frame's initial depth buffer. This leverages temporal coherence to potentially save rendering cost and avoid slight inaccuracies or latency with very fast-moving objects, which wouldn't be the case if reprojection from the previous frame depth buffer would be done here.

2.  **Hi-Z Buffer Construction:** 
    Using the depth buffer generated in the previous step, construct the Hi-Z mipmap pyramid. This is typically done using compute shaders or specialized hardware features. Starting from the full-resolution depth buffer (Mip 0), successive downsampling passes are performed. In each pass, a compute shader reads a block of texels (e.g., 2x2) from the source mip level and writes the *minimum* (nearest) Z value among them to the corresponding single texel in the destination (coarser) mip level. This process repeats until the smallest mip level (often 1x1) is generated.

#### Pass 2: Occlusion Query Pass

This pass determines the visibility of the remaining objects (potential *occludees*) that were not rendered in the initial Z-prepass.

1.  **Prepare Occludee List:** Identify the objects whose visibility needs testing. This usually involves objects that passed view-frustum culling. For each object, its bounding volume (e.g., an axis-aligned bounding box - AABB, or an oriented bounding box - OBB) is used for the test.
2.  **GPU-Based Testing:** This pass is typically implemented using compute shaders or by drawing bounding boxes and using hardware occlusion queries. For each potential occludee:
    * **Project Bounding Box:** Project the vertices of the object's bounding box onto the screen space.
    * **Find Screen-Space Rectangle:** Determine the screen-space rectangle that encloses the projected bounding box.
    * **Determine Farthest Depth ($Z_{max}$):** Calculate the maximum Z value (farthest point from the camera) of the bounding box's vertices after projection. This value is in clip space or a similar space compatible with the depth buffer.
    * **Select Hi-Z Mip Level:** Based on the screen-space size of the projected bounding box, select an appropriate mip level from the Hi-Z buffer. Smaller projections can use coarser (higher) mip levels for faster queries.
    * **Sample Hi-Z:** Sample the Hi-Z buffer within the screen-space rectangle corresponding to the object's projection, using the selected mip level. The goal is to find the *minimum* depth value ($Z_{HiZ}$) stored in the Hi-Z within that region. This represents the nearest occluder surface already rendered in that area during Pass 1.
    * **Perform Depth Test:** Compare the object's farthest depth ($Z_{max}$) with the nearest depth found in the Hi-Z ($Z_{HiZ}$).
        * **If $Z_{max} > Z_{HiZ}$:** The farthest part of the object's bounding box is behind the nearest occluder found in that screen region. Therefore, the object is considered **occluded**.
        * **If $Z_{max} \le Z_{HiZ}$:** The object's bounding box is potentially in front of or intersecting the nearest known occluder. The object is considered **potentially visible**.
3.  **Generate Visibility Results and Indirect Draw Commands:** 
    The results of these tests (a list or buffer indicating whether each object is visible or occluded) are stored on the GPU. This visibility information is then used in the main rendering pass to draw only the objects that are potentially visible. Additionally, indirect draw commands 
    are written to a buffer for the objects that passed the occlusion test. This buffer contains the draw commands for the visible objects, which will be used in the main rendering passes. The indirect draw command buffer is a GPU-side structure that allows for efficient rendering of multiple objects in a single draw call, reducing CPU overhead and improving performance. 
4.  **Render Rest Of Visible Objects Into the Depth Buffer:** 
    The objects that are not occluded (i.e., those that passed the depth test) are rendered into the same depth buffer as well, for to construct the final depth buffer for the main rendering passes, so that early depth testing (early-Z testing) can be used to skip over occluded fragments in the main rendering pass. This is important for performance, as it allows the GPU to skip processing fragments that are occluded by other objects.

### Main Rendering Pass

Finally, the main rendering passes draws the scene on the screen using the visibility results from the previous occlusion query pass in one go. This is typically done using indirect draw commands, which allow the GPU to efficiently render multiple objects in a single draw call or few draw calls, reducing CPU overhead and improving performance. The indirect draw command buffer generated in the previous occlusion query pass is used to issue the draw calls for the visible objects. This allows the GPU to efficiently render only the objects that are potentially visible, skipping over those that were determined to be occluded in the previous pass. Shader optimizations can allow reusing the depth from the prepass to avoid redundant depth calculations (Equal Depth Test).

### Advantages

* **GPU Driven:** Performs culling directly on the GPU, avoiding CPU bottlenecks and costly GPU-CPU data transfers (read-backs).
* **Efficient for Many Objects:** Scales well with a high number of potential occludees.
* **Handles Dynamic Scenes:** By regenerating the Hi-Z buffer each frame (or frequently), it correctly handles moving occluders and occludees.
* **Conservative:** The use of bounding boxes and minimum depth values in the Hi-Z buffer ensures that objects are generally not culled incorrectly. False negatives are rare or even completely avoided when implemented correctly. However, the technique may occasionally classify occluded objects as visible, resulting in false positives. This is acceptable and even preferable, since rendering a few unnecessary objects (overdraw) is safer than missing visible ones.
* **No Latency:** Because no reprojection is involved, the Hi-Z buffer accurately reflects with the current frame's geometry at all times. This offers a key advantage over older Hi-Z techniques based on reprojection but following a similar overall concept, which could suffer from latency and precision loss in scenarios involving high-velocity motion, object transformations or rapid geometric alterations. By directly deriving the Hi-Z buffer from the present frame, this approach ensures real-time, accurate occlusion without the artifacts inherent in reprojecting previous depth data, crucial for maintaining visual fidelity in demanding scenarios.

### Disadvantages

* **Overhead:** Requires a Z-prepass (adds draw calls) and compute resources for Hi-Z generation and the query pass. May not be beneficial in simple scenes where occlusion is minimal, but it doesn't hurt either on modern GPUs as they can handle it well.
* **Memory Usage:** The Hi-Z buffer requires additional memory, especially for high-resolution textures. This can be a concern on lower-end hardware or when many mip levels are used.
* **Potential Inaccuracy:** Testing against bounding boxes and/or bounding spheres is an approximation, where objects might be marked visible even if technically occluded, especially with complex shapes or poorly fitting bounding boxes. However, this conservative approach is intended to prevent false negatives, making the resulting false positives generally acceptable in real-time rendering, unlike false negatives which would indicate incorrect implementations.
* **Complexity:** Implementation requires careful handling of coordinate spaces, depth precision, and GPU synchronization.

### Conclusion

Hi-Z Two-Pass Occlusion Culling is a powerful, GPU-centric technique for improving rendering performance in complex 3D scenes. By building a hierarchical representation of the depth buffer and using it to perform fast visibility tests on the GPU before the main rendering pass, it effectively skips the processing and rendering of objects hidden from view, freeing up GPU resources for visible geometry and effects.
