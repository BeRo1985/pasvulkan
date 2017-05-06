# pasvulkan

Vulkan header generator and Vulkan OOP-style API wrapper for Object Pascal (FreePascal >= 3.0.2 and Delphi >= 7.0)

It includes also a best-fit red-black-tree based memory manager for the Vulkan memory management, for to manage the sub-allocations in the allocated buffers and for to keep the total count of simultaneous live allocations as much low as possible and less than TVkPhysicalDeviceLimits.maxMemoryAllocationCount.
