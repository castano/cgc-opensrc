/****************************************************************************\
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
\****************************************************************************/
// generic_hal.h
//

#if !defined(__GENERIC_HAL_H)
#define __GENERIC_HAL_H 1

#include "hal.h"

// Vendor and version strings:

#define VENDOR_STRING_GENERIC        "NVIDIA Corporation"
#define VERSION_STRING_GENERIC       "1.0.1"

// Generic connectors:

#define CID_GENERIC_IN_NAME          "generic_in"
#define CID_GENERIC_IN_ID            8

#define CID_GENERIC_OUT_NAME         "generic_out"
#define CID_GENERIC_OUT_ID           9

// Generic profile:

#define PROFILE_GENERIC_NAME         "generic"
#define PROFILE_GENERIC_ID           5


enum regAppToVertex_generic { 
    REG_AP2V_ATTR0, REG_AP2V_ATTR1, REG_AP2V_ATTR2, REG_AP2V_ATTR3,
    REG_AP2V_ATTR4, REG_AP2V_ATTR5, REG_AP2V_ATTR6, REG_AP2V_ATTR7,
    REG_AP2V_ATTR8, REG_AP2V_ATTR9, REG_AP2V_ATTR10, REG_AP2V_ATTR11,
    REG_AP2V_ATTR12, REG_AP2V_ATTR13, REG_AP2V_ATTR14, REG_AP2V_ATTR15,
};

enum regVertexToFrag_generic { 
    REG_V2FR_HPOS, 
    REG_V2FR_COL0, REG_V2FR_COL1,
    REG_V2FR_TEX0, REG_V2FR_TEX1, REG_V2FR_TEX2, REG_V2FR_TEX3,
    REG_V2FR_FOGC, REG_V2FR_PSIZ,
    REG_V2FR_WPOS,
};

int RegisterProfiles_generic(void);

#endif // !defined(__GENERIC_HAL_H)
