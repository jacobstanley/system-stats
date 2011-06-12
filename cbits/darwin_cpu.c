#include <mach/mach.h>
#include <mach/mach_error.h>
#include <stdlib.h>

#include "darwin_cpu.h"


int c_get_cpu_usage(cpu_usage_t **usage_array, int *count)
{
    *count = 0;
    *usage_array = 0;

  	natural_t cpu_count;
	processor_cpu_load_info_data_t *load_info;
	mach_msg_type_number_t info_count;

	kern_return_t error = host_processor_info(
            mach_host_self(),
            PROCESSOR_CPU_LOAD_INFO,
            &cpu_count,
            (processor_info_array_t *) &load_info,
            &info_count);

	if (error) {
		return error;
	}

    *count = cpu_count;
    *usage_array = (cpu_usage_t *) malloc(sizeof(cpu_usage_t) * cpu_count);

    int cpu;
	for (cpu = 0; cpu < cpu_count; cpu++) {
        unsigned int *ticks = load_info[cpu].cpu_ticks;

        (*usage_array)[cpu].user   = ticks[CPU_STATE_USER];
        (*usage_array)[cpu].system = ticks[CPU_STATE_SYSTEM];
        (*usage_array)[cpu].idle   = ticks[CPU_STATE_IDLE];
        (*usage_array)[cpu].nice   = ticks[CPU_STATE_NICE];
    }

	vm_deallocate(
            mach_task_self(),
            (vm_address_t) load_info,
            info_count);

	return 0;
}


void c_free_cpu_usage(cpu_usage_t *usage_array)
{
    free(usage_array);
}
