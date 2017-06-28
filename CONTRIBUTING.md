
# General guidelines for code contributors                 
 
1. Make sure you are legally allowed to make a contribution under the zlib license.
2. The zlib license header goes at the top of each source file, with appropriate copyright notice.
3. This PasVulkan wrapper may be used only with the PasVulkan-own Vulkan Pascal header.
4. After a pull request, check the status of your pull request on http://github.com/BeRo1985/pasvulkan
5. Write code, which is compatible with Delphi 7-XE7 and FreePascal >= 3.0 so don't use generics/templates, operator overloading and another newer syntax features than Delphi 7 has support for that, but if needed, make it out-ifdef-able.
6. Don't use Delphi-only, FreePascal-only or Lazarus-only libraries/units, but if needed, make it out-ifdef-able.
7. No use of third-party libraries/units as possible, but if needed, make it out-ifdef-able.
8. Try to use const when possible.
9. Make sure to comment out writeln, used while debugging.
10. Make sure the code compiles on 32-bit and 64-bit platforms (x86-32, x86-64, ARM, ARM64, etc.).
11. Make sure the code runs on all platforms with Vulkan support
