#ifndef __DRIVERCALL_H
#define __DRIVERCALL_H

#include "types.h"

void DriverCall(byte slot, uint routineAddress);
void DosCall(byte function, Z80_registers* regs, register_usage inRegistersDetail, register_usage outRegistersDetail);
void DosCallFromRom(byte function, register_usage outRegistersDetail);
void SwitchSystemBankThenCall(int routineAddress, register_usage outRegistersDetail);

#endif
