-------------------
RELEASE INFORMATION
-------------------

This release builds the Cg compiler (cgc.exe) with the "generic" profile,
which does some minimal semantic checks and prints out a tree representation
of the code.  It can be built either with the included Microsoft Visual C++ 6.0 projects
and workspace, or with the included Makefile.

The release contains a pre-built parser (parser.c and parser.h) built
from parser.y with GNU bison.  Rebuilding will require GNU bison if
you make any changes to parser.y

This release contains the front-end to the compiler, plus a 'trivial'
back-end (profile) that does some limited checking and outputs a parse tree
to stdout. This back-end is almost entirely encapsulted in generic_hal.[ch]

hal.[ch] describes the hardware abstraction layer by which profiles
communicate with the front-end.  To add a new profile, you can use
generic_hal.c as a framework/example.  A profile must register itself
with the HAL by calling RegisterProfile as part of compiler startup.
Then, if selected via the appropriate command line argument, the
profile will be called to verify various constructs in the source code,
deal with connector semantics, and finally, generate code.  See
generic_hal.c for more details. 

The directory contains 4 examples: vertexlight.cg, vertexlight4.cg,
position.cg, and reflection.cg. After building cgc.exe, you can use
following command line to compile these: 
  cgc -profile generic <example_file_name>
This release prints the output to stdout only.

Developers can download all the latest Cg-related content from the Cg 
website at: 

http://www.nvidia.com/Cg

-------
LICENSE
-------

Copyright (c) 2002, NVIDIA Corporation.

NVIDIA Corporation("NVIDIA") supplies this software to you in
consideration of your agreement to the following terms, and your use,
installation, modification or redistribution of this NVIDIA software
constitutes acceptance of these terms.  If you do not agree with these
terms, please do not use, install, modify or redistribute this NVIDIA
software.

In consideration of your agreement to abide by the following terms, and
subject to these terms, NVIDIA grants you a personal, non-exclusive
license, under NVIDIA's copyrights in this original NVIDIA software (the
"NVIDIA Software"), to use, reproduce, modify and redistribute the
NVIDIA Software, with or without modifications, in source and/or binary
forms; provided that if you redistribute the NVIDIA Software, you must
retain the copyright notice of NVIDIA, this notice and the following
text and disclaimers in all such redistributions of the NVIDIA Software.
Neither the name, trademarks, service marks nor logos of NVIDIA
Corporation may be used to endorse or promote products derived from the
NVIDIA Software without specific prior written permission from NVIDIA.
Except as expressly stated in this notice, no other rights or licenses
express or implied, are granted by NVIDIA herein, including but not
limited to any patent rights that may be infringed by your derivative
works or by other works in which the NVIDIA Software may be
incorporated. No hardware is licensed hereunder. 

THE NVIDIA SOFTWARE IS BEING PROVIDED ON AN "AS IS" BASIS, WITHOUT
WARRANTIES OR CONDITIONS OF ANY KIND, EITHER EXPRESS OR IMPLIED,
INCLUDING WITHOUT LIMITATION, WARRANTIES OR CONDITIONS OF TITLE,
NON-INFRINGEMENT, MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE, OR
ITS USE AND OPERATION EITHER ALONE OR IN COMBINATION WITH OTHER
PRODUCTS.

IN NO EVENT SHALL NVIDIA BE LIABLE FOR ANY SPECIAL, INDIRECT,
INCIDENTAL, EXEMPLARY, CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED
TO, LOST PROFITS; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) OR ARISING IN ANY WAY
OUT OF THE USE, REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION OF THE
NVIDIA SOFTWARE, HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT,
TORT (INCLUDING NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF
NVIDIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

