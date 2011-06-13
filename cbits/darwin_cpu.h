#pragma once


/**
 * The usage info of a single processor since boot time.
 */
typedef struct cpu_usage {
    unsigned int user;    /* ticks executed by normal processes */
    unsigned int system;  /* ticks executed by system processes */
    unsigned int idle;    /* ticks spent idle */
    unsigned int nice;    /* ticks executed by nice processes */
} cpu_usage_t;


/**
 * Gets the usage info for each processor since boot time.
 *
 * usage_array [out] the array of usage info (one per CPU)
 * count       [out] the number of CPUs
 */
int get_cpu_usage(cpu_usage_t **usage_array, int *count);


/**
 * Frees the array allocated by 'c_get_cpu_usage'.
 *
 * usage_array [in] the array allocated by 'c_get_cpu_usage'
 */
void free_cpu_usage(cpu_usage_t *usage_array);
