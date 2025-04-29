#include <windows.h>
#include <stdio.h>
#include <stdlib.h>

/* Here's an overview of how support for waiting more than 64 objects
   on MS-Windows.

   As noted in the MS documentation, WaitForMultipleObjects can wait on
   a maximum of MAXIMUM_WAIT_OBJECTS (64) objects. Due to this
   limitation, earlier versions of Emacs (at least 30) could only
   support up to 32 subprocesses or network connections.

   The documentation suggests using multiple threads or a thread pool to
   wait on a larger number of objects. With Windows 2000, Microsoft has
   added new thread pooling functions to make thread creation,
   destruction, and general management easier:

   . https://jacobfilipp.com/MSJ/pooling.html

   Thread pools implement waiting by calling WaitForMultipleObjects or
   relying on completion ports (Windows 8 or above) in their internal
   threads, as The Old New Thing said:

   . https://devblogs.microsoft.com/oldnewthing/20081117-00/?p=20183
   . https://devblogs.microsoft.com/oldnewthing/20220406-00/?p=106434

   However, since the system thread pool in versions prior to Windows 7
   does not support adjusting thread stack sizes by calling
   SetThreadpoolStackInformation, using a large number of threads may
   consume a lot of address space. This can have a significant impact on
   the limited 2GB address space available on 32-bit systems. To avoid
   this issue on Windows Vista and earlier systems and make the code as
   generic as possible, a simple waiting thread pool is manually
   implemented here.

   The waiting thread pool contains a maximum of 32 threads, with each
   thread capable of waiting on up to 63 objects (one object is reserved
   for communication with the main thread). Combined with the main
   thread's WaitForMultipleObjects call, this implementation supports
   waiting on a maximum of 2048 objects. Since Windows only supports
   creating subprocesses using 'pipe method, this allows Emacs to create
   a maximum of approximately 1024 subprocesses. This limit is close to
   the number of subprocesses that can be created using the 'pty method
   on Linux when the default FD_SETSIZE is 1024.

   Once created, the threads do not exit after finish waiting; instead,
   they enter an infinite loop, waiting for an event to trigger and
   begin their WaitForMultipleObjects tasks, thereby eliminating the
   additional time and power consumption associated with frequently
   creating and destroying threads. If a thread is not triggered after a
   certain number of sys_select calls, it can be terminated to free up
   resources. */

/* The values returned by WaitForMultipleObjects, WAIT_ABANDONED_0
   (0x80) and WAIT_TIMEOUT (0x102), are less than 2048 (0x800), so
   define new constants to replace them. 'WFO' prefix stands for
   'WaitForObjects'. */
#define WFO_ABANDONED 0x10000
#define WFO_TIMEOUT   0x20002
#define WFO_FAILED    0xfffff
#define WFO_MAX_WAIT  FD_SETSIZE

/* Structure for waiting thread's context. */
typedef struct
{
  /* Handle to the thread itself. */
  HANDLE thread;
  /* Handle to an event object that is signaled by the main thread
     to tell thread start waiting. */
  HANDLE wait_start;
  /* Handle to an event object that is signaled when this thread is not
     used for a period of time, it tells thread to quit. */
  HANDLE wait_quit;
  /* Handle to an event object that is signaled when wokrer thread is
     ready to call WaitForMultipleObjects on the given objects. */
  HANDLE wait_ready;
  /* pHandles and nCount are part of the WaitForMultipleObjects
     parameters, specifying the array of objects to wait on and the
     number of objects. bWaitAll and dwMilliseconds are not needed. */
  HANDLE *pHandles;
  int    nCount;
  /* The return value of the thread's call to WaitForMultipleObjects. */
  DWORD  result;
  /* Used to store GetLastError value of failed wait. */
  DWORD  errcode;
  /* The thread's wait count. */
  int    call_count;
} wait_objects_context;

/* Structure for the waiting thread pool */
typedef struct
{
  /* An array for the context of each worker thread. */
  wait_objects_context wocs[32];
  /* The number of the current threads for waiting. */
  int                  count;
  /* times of waiting in main thread. */
  int                  main_thread_waits;
  /* The event handle that signals the worker thread's timeout. */
  HANDLE               timeout_event;
  /* The flag that indicates whether the thread pool is initialized. */
  int                  init_flag;
} wait_objects_pool;

/* Structure for information about the grouping of waits. */
typedef struct
{
  /* An array of handles used as parameters for WaitForMultipleObjects
     in the main thread, which includes event handles wait_ready
     signaled by the worker threads, as well as any possible remaining
     wait objects. */
  HANDLE hs[64];
  /* The number of threads to be used. */
  int    nt;
  /* The number of handles the main thread needs to wait for. */
  int    nh;
} wait_objects_info;

/* Declare wait_pool and wait_info as static variables to avoid
   unnecessary stack allocation. */
static wait_objects_pool wait_pool;
static wait_objects_info wait_info;

/* Thread proc for worker threads waiting on objects. Each thread is
   normally blocked until woken by the main thread to wait on objects.
   When the wait completes, wait_ready is signaled to wake up the main
   thread and the thread blocks itself again.

   The worker thread needs to wait for the timeout event from the main
   thread. When timeout_event is signaled by the main thread, it will
   end the current wait and start the next round. Similarly, threads
   also need to wait for an quit event, which usually occurs when the
   thread has not been used for a certain period of time. */
static DWORD WINAPI
wait_objects_thread (void *arg)
{
  wait_objects_context *ctx = NULL;
  /* Thread's wait handle array. */
  HANDLE hs[64] = {NULL};
  /* start and quit event handle array. */
  HANDLE start_or_quit[2] = {NULL};
  /* return value of waiting. */
  DWORD res = 0;
  /* Thread's context. */
  ctx = (wait_objects_context *) arg;
  /* Place start event in start_or_quit[0]. */
  start_or_quit[0] = ctx->wait_start;
  /* Place quit event in start_or_quit[1]. */
  start_or_quit[1] = ctx->wait_quit;
  for (;;)
    {
      /* increase thread's wait call count by 1. */
      ctx->call_count++;
      /* The timeout event object is placed at the end.
	 nCount will not exceed 63. */
      hs[ctx->nCount] = wait_pool.timeout_event;
      /* The contents copied by different threads do not overlap, so it
	 is safe to use memcpy. */
      memcpy (hs, ctx->pHandles, ctx->nCount * sizeof (HANDLE));
      /* Start waiting. */
      ctx->result = WaitForMultipleObjects (ctx->nCount + 1, hs,
					    FALSE, INFINITE);
      /* Get the error code when the wait fails. */
      if (ctx->result == WAIT_FAILED)
	{
	  ctx->errcode = GetLastError ();
	  SetEvent (ctx->wait_ready);
	  return WFO_FAILED;
	}
      /* After waiting, signal wait_ready and wait for the wait_start
	 and wait_quit events from the main thread. */
      if (!SetEvent (ctx->wait_ready))
	{
	  ctx->errcode = GetLastError ();
	  return WFO_FAILED;
	}
      res = WaitForMultipleObjects (2, start_or_quit, FALSE, INFINITE);
      switch (res)
	{
	  /* wait_start, continue loop. */
	case WAIT_OBJECT_0:
	  break;
	  /* wait_quit, exit with code 0. */
	case WAIT_OBJECT_0 + 1:
	  /* Close its wait_quit event handle. */
	  if (!CloseHandle (start_or_quit[1]))
	    return GetLastError ();
	  return 0;
	  /* Failure to wait for the start and quit event from the main
	     thread should not occur. */
	case WAIT_ABANDONED_0:
	case WAIT_ABANDONED_0 + 1:
	case WAIT_TIMEOUT:
	case WAIT_FAILED:
	  ctx->errcode = GetLastError ();
	  return WFO_FAILED;
	}
    }
  return 0;
}

/* Determine the grouping based on the given wait objects and assign
   them to the worker threads to begin waiting, creating new worker
   threads if necessary. The function also performs initialization of
   some static resources. */
static int
start_wait_objects (wait_objects_info *p,
		    DWORD nCount,
		    HANDLE *lpHandles)
{
  /* Initialization of static resources. */
  if (!wait_pool.init_flag)
    {
      wait_pool.timeout_event  = CreateEvent (NULL, TRUE, FALSE, NULL);
      /* If resource initialization fails, exit immediately. */
      if (!wait_pool.timeout_event)
	return WFO_FAILED;
      /* Set init_flag. */
      wait_pool.init_flag = TRUE;
    }
  /* Check if all threads in the thread pool are working properly. If
     any thread has exited, exit immediately. */
  for (int i = 0; i < wait_pool.count; i++)
    {
      /* The MS documentation suggests that callers should call the
	 GetExitCodeThread function only after the thread has been
	 confirmed to have exited. Use the WaitForSingleObject with a
	 wait duration of zero to determine whether a thread has
	 exited. */
      DWORD res = WaitForSingleObject (wait_pool.wocs[i].thread, 0);
      /* Thread is alive. */
      if (res == WAIT_TIMEOUT)
        continue;
      /* The thread unexpectedly terminated when waiting
	 wait_start and wait_quit. */
      else if (res == WAIT_OBJECT_0)
        {
	  /* The last-error code is kept in thread local storage so that
	     multiple threads do not overwrite each other's values. Pass
	     the error by using SetLastError to set the error value. */
	  SetLastError (wait_pool.wocs[i].errcode);
	  return WFO_FAILED;
	}
      /* Unexpected return value from WaitForSingleObject.
         WAIT_FAILED or WAIT_ABANDONED. */
      else
	return WFO_FAILED;
    }
  /* Calculate the required number of threads and the number of objects
     the main thread needs to wait for based on the number of objects to
     be waited on. To avoid increasing the number of threads for a small
     increase in the number of objects (e.g., using two threads for 65
     objects), the following calculation allows the main thread's array
     to hold up to 32 wait objects, excluding the thread event
     handles. With this approach, the minimum number of wait objects for
     the worker threads is 32, and it allows a maximum of 63 * 32 + 32 =
     2048 objects to be waited on.

     The number of child process in Emacs is limited by MAX_CHILDREN,
     which is half of MAXDESC (FD_SETSIZE). When FD_SETSIZE is set to
     2048, a maximum of 1024 child processes and network/serial are
     allowed. Compared to network/serial, the process handles of child
     processes are also waited on, which is why only a maximum of 1024
     child processes can be created.

     When there are 1024 child processes, the main thread needs to wait
     for 64 objects, which seems to exceed the 63 allowed by
     MsgWaitForMultipleObjects. However, due to the influence of
     emacs_pipe, a maximum of only 1021 child processes can be created,
     so the number of elements in the main thread's wait array will not
     exceed 63. The explanation is as follows:

     . emacs_pipe calls the pipe2 function located in w32.c.
     . pipe2 makes sure that file descriptors return by _pipe less than
       MAXDESC (2048), Therefore, the range of available file descriptor
       values is from 3 to 2047 (0, 1, 2 for stdin, stdout and
       stderr). Since pipe file descriptors are opened in pairs, the
       actual range is from 3 to 2046, meaning there are 2044 available
       file descriptors.
     . When create_process creates a process using the 'pipe' method, it
       creates two pipes, one for reading and one for writing. It then
       closes one file descriptor for each pipe, as each pipe is only
       used for reading or writing. This results in 4 file descriptors
       being used initially for each process creation, and then 2 are
       released. When only 2 empty slots remain, no new child processes
       can be created.
     . In the end, we can use 2042 file descriptors, which allows for
       1021 child processes. */

  /* Compared to using (nCount / 63), we use (nCount - 33) / 63, which
     will cause a new thread to be added only when the number of wait
     objects in the main thread's array exceeds 32. */
  p->nt = 1 + (nCount - 33) / 63;
  /* The number of objects the main thread needs to wait for is the
     required number of threads plus the remaining objects. If the
     existing threads are sufficient to wait for all the objects, then
     nh is the number of threads. */
  p->nh = (p->nt * 63 >= nCount) ? p->nt : (p->nt + nCount - p->nt * 63);
  /* In the main thread's wait array hs, the first nt are thread event
     handles, and the remaining are the remaining objects. */
  if (p->nh != p->nt)
    memcpy (p->hs + p->nt, lpHandles + p->nt * 63,
	    sizeof (HANDLE) * (nCount - p->nt * 63));
  /* Set the pHandles and nCount parameters for each thread. Since the
     waiting task is relatively simple, we do not need a dedicated
     task queue mechanism. We can simply wait for the corresponding
     objects in the order of the thread context array wait_pool.wocs. */
  for (int i = 0; i < p->nt; i++)
    {
      /* If it is the last thread and the thread is sufficient to wait
	 all the objects, then the count needs to be calculated;
	 otherwise, it will be 63. */
      int count = (i == p->nt - 1)
	? (p->nt == p->nh) ? (nCount - i * 63) : 63 : 63;
      wait_pool.wocs[i].nCount = count;
      wait_pool.wocs[i].pHandles = lpHandles + i * 63;
    }
  /* Get current threads count. */
  int orig_thread_count = wait_pool.count;
  /* If the current thread pool is insufficient to wait for all the
     objects, create new threads and initialize the event handles. */
  while (wait_pool.count < p->nt)
    {
      wait_objects_context *ctx = wait_pool.wocs + wait_pool.count;
      /* Create wait_start, wait_quit and wait_ready events. */
      ctx->wait_start = CreateEvent (NULL, FALSE, FALSE, NULL);
      if (!ctx->wait_start)
	return WFO_FAILED;
      ctx->wait_quit  = CreateEvent (NULL, FALSE, FALSE, NULL);
      if (!ctx->wait_quit)
	return WFO_FAILED;
      /* The wait_ready event is set to be manually reset because it may
	 be waited on multiple times. */
      ctx->wait_ready = CreateEvent (NULL, TRUE, FALSE, NULL);
      if (!ctx->wait_ready)
	return WFO_FAILED;
      /* Set call_count and errcode to ZERO. */
      ctx->call_count = 0;
      ctx->errcode = 0;
      /* Creating new worker threads. Please refer to the comment in
	 w32proc.c within the new_child function, where the
	 reader_thread thread is created, to understand why these
	 parameters are needed. */
      ctx->thread = CreateThread (NULL, 64 * 1024, wait_objects_thread,
				  ctx, 0x00010000, NULL);
      /* CreateThread failed. */
      if (!ctx->thread)
	return WFO_FAILED;
      wait_pool.count++;
    }
  /* Fill the main thread's wait array with the wait_ready event
     handles of the required worker threads, and set them to be
     unsignaled. */
  for (int i = 0; i < p->nt; i++)
    {
      p->hs[i] = wait_pool.wocs[i].wait_ready;
      /* Reset the wait_ready event and set wait_start event for threads
	 that are not newly created. Newly created threads start
	 execution immediately. */
      if (i < orig_thread_count)
	if (ResetEvent (wait_pool.wocs[i].wait_ready)
	    && SetEvent (wait_pool.wocs[i].wait_start))
	  continue;
        /* SetEvent or ResetEvent failed. */
	else
	  return WFO_FAILED;
    }
  return 0;
}

/* Ensure that all worker threads have completed their wait and are in
   the ready state. */
static int
stop_wait_objects (wait_objects_info *p)
{
  /* Set timeout_event to tell all worker threads stop waiting. */
  if (!SetEvent (wait_pool.timeout_event))
    return WFO_FAILED;
  /* Wait for all the used worker threads to signal wait_ready. This
     typically takes no more than a few dozen microseconds, so a
     timeout indicates that an error has occurred. */
  DWORD result = WaitForMultipleObjects (p->nt, p->hs, TRUE, 20);
  if (result == WAIT_FAILED || result == WAIT_ABANDONED)
    return WFO_FAILED;
  /* Timeout means there exists a thread exit abnormally. Since the
     WAIT_FAILED error in the worker threads signals wait_ready, this
     can only be an error occurring when the worker threads are waiting
     for curr_start and wait_quit. */
  if (result == WAIT_TIMEOUT)
    {
      /* Find the first dead thread. */
      for (int i = 0; i < p->nt; i++)
	{
	  if (WaitForSingleObject (wait_pool.wocs[i].thread, 0) == WAIT_OBJECT_0)
	    {
	      SetLastError (wait_pool.wocs[i].errcode);
	      return WFO_FAILED;
	    }
	}
      return WFO_FAILED;
    }
  /* Even if the wait succeeds, there is still a possibility that some
     wait_ready might be signaled by a WAIT_FAILED error. To determine
     if no WAIT_FAILED error occurred, check the errcode of all threads. */
  for (int i = 0; i < p->nt; i++)
    {
      if (wait_pool.wocs[i].errcode)
	{
	  SetLastError (wait_pool.wocs[i].errcode);
	  return WFO_FAILED;
	}
    }
  /* Reset the timeout event. */
  if (!ResetEvent (wait_pool.timeout_event))
    return WFO_FAILED;
  return 0;
}

/* After the main thread finishes waiting, obtain the signaled object
   index based on the main thread's return value, or other possible
   return values. */
static DWORD
end_wait_and_return (wait_objects_info *p, DWORD result)
{
  /* Wait timeout in main thread. */
  if (result == WAIT_TIMEOUT)
    result = WFO_TIMEOUT;
  /* Wait failed in main thread. */
  else if (result == WAIT_FAILED)
    return WFO_FAILED;
  /* It seems that all the wait objects are just process handles and
     event handles. WAIT_ABANDONED may not occur, but let's just
     handle it here. */
  else if (result >= WAIT_ABANDONED_0
	   && result < p->nh + WAIT_ABANDONED_0)
    {
      result -= WAIT_ABANDONED_0;
      /* The event object wait_ready is not mutex object. This is
	 unlikely to happen... */
      if (result < p->nt)
	return WFO_FAILED;
      /* There are 62 * nt objects before the objects in the main
	 thread. Each thread can wait for 63 objects, and each
	 thread's wait_ready occupies one position in the main thread
	 array. Therefore, the index of the waiting object in the main
	 thread array should be incremented by (63 - 1) multiplied by
	 the number of worker threads. We add WFO_ABANDONED to
	 distinguish it from normal object index. */
      result = result + 62 * p->nt + WFO_ABANDONED;
    }
  /* Wait succeed in main thread. */
  else
    {
      /* Object index in main thread wait array. */
      int idx = result - WAIT_OBJECT_0;
      /* Object is in main thread's wait array. */
      if (idx >= p->nt)
	{
	  /* When the index is equal to the number of worker threads,
	   and the number of worker threads is equal to the number of
	   wait objects for the main thread, it indicates that the main
	   thread is using MsgWaitForMultipleObjects and that there are
	   no wait objects in the main thread's array. We should return
	   the total number of wait objects to indicate that a message
	   was received during the wait. */
	  if (p->nt == p->nh)
	    result = (idx - 1) * 63 + wait_pool.wocs[idx - 1].nCount;
	  /* Normal case. */
	  else
	    result = 62 * p->nt + idx;
	}
      /* Object is in worker threads. */
      else
        {
	  DWORD t_result = wait_pool.wocs[idx].result;
	  /* WAIT_FAILED or WAIT_TIMEOUT. Since the wait time in the
	     worker thread is set to INFINITE, WAIT_TIMEOUT should not
	     occur. */
          if (t_result == WAIT_FAILED || t_result == WAIT_TIMEOUT)
	    {
	      SetLastError (wait_pool.wocs[idx].errcode);
	      return WFO_FAILED;
	    }
	  /* Abandoned. Compared to the waiting objects in the main
	     thread's array, the index in the worker threads needs to be
	     incremented by 63 times the number of preceding threads,
	     rather than 62. */
          else if (t_result >= WAIT_ABANDONED_0
		   && t_result < wait_pool.wocs[idx].nCount
		      + WAIT_ABANDONED_0)
            result = idx * 63 + t_result
	      + WFO_ABANDONED - WAIT_ABANDONED_0;
	  /* The worker thread is signaled by timeout_event, but at this
	     point, timeout_event has not yet been signaled. */
	  else if (t_result == wait_pool.wocs[idx].nCount + WAIT_OBJECT_0)
	    return WFO_FAILED;
	  /* Normal object index. */
	  else
	    result = idx * 63 + (t_result - WAIT_OBJECT_0);
	}
    }
  /* Ensure that all the worker threads are ready to begin the next
     round of waiting, and check for any potential errors. */
  if (stop_wait_objects (&wait_info) == WFO_FAILED)
    return WFO_FAILED;
  return result;
}

/* Check if there are inactive worker threads in the waiting thread
   pool and let them exit. */
static int
shrink_wait_pool (void)
{
  wait_pool.main_thread_waits++;
  /* Check the thread every 64 Wait call. 64 is just a value that might
     be reasonably suitable. */
  if (wait_pool.main_thread_waits <= 64)
    return 0;
  wait_pool.main_thread_waits = 0;
  /* return if no thread. */
  if (wait_pool.count == 0)
    return 0;
  /* Each time, we only check the last worker thread, which helps avoid
     terminating a large number of threads at the same time. */
  int last = wait_pool.count - 1;
  /* A call count of 0 indicates that the thread has not been used
     during the past period of time. */
  if (wait_pool.wocs[last].call_count == 0)
    {
      /* Signal wait_quit to let the worker thread exit. */
      if (!SetEvent (wait_pool.wocs[last].wait_quit))
	return WFO_FAILED;
      wait_pool.wocs[last].wait_quit = NULL;
      /* Close thread handle. */
      if (!CloseHandle (wait_pool.wocs[last].thread))
	return WFO_FAILED;
      wait_pool.wocs[last].thread = NULL;
      /* Close wait_start event handle. */
      if (!CloseHandle (wait_pool.wocs[last].wait_start))
	return WFO_FAILED;
      wait_pool.wocs[last].wait_start = NULL;
      /* Close wait_ready event handle */
      if (!CloseHandle (wait_pool.wocs[last].wait_ready))
	return WFO_FAILED;
      wait_pool.wocs[last].wait_ready = NULL;
      /* decrease the number of workder thread by 1. */
      wait_pool.count--;
    }
  /* Reset call_count for each worker thread. */
  for (int i = 0; i <= last; i++)
    wait_pool.wocs[i].call_count = 0;
  return 0;
}

/* Exit all worker threads and release the start and timeout
   handles. This function is called when exiting Emacs. */
void
free_wait_pool (void)
{
  /* Emacs has never used more than 32 child processes. */
  if (wait_pool.init_flag == FALSE)
    return;
  for (int i = 0; i < wait_pool.count; i++)
    {
      /* Send quit event to earch worker thread. */
      SetEvent (wait_pool.wocs[i].wait_quit);
      /* Close wait_start, wait_ready event. */
      CloseHandle (wait_pool.wocs[i].wait_start);
      CloseHandle (wait_pool.wocs[i].wait_ready);
    }
  /* Wait all workder threads exit. Stop if failed. */
  for (int i = 0; i < wait_pool.count; i++)
    {
      if (WaitForSingleObject (wait_pool.wocs[i].thread, 1)
	  != WAIT_OBJECT_0)
	break;
    }
  /* Close timeout event. */
  CloseHandle (wait_pool.timeout_event);
  return;
}

/* Replacement of WaitForMultipleObjects, with the bWaitAll parameter
   removed. The function's return values are as follows:

   [0 ~ nCount-1], the return value indicates the lpHandles array
   index of the object that satisfied the wait.

   [WFO_ABANDONED ~ nCount-1 + WFO_ABANDONED], the return value minus
   WFO_ABANDONED indicates the lpHandles array index of an abandoned
   mutex object that satisfied the wait.

   [WFO_TIMEOUT], The time-out interval elapsed.

   [WFO_FAILED], The function has failed. To get extended error
   information, call GetLastError. */
static DWORD
wait_for_objects (DWORD nCount, HANDLE *lpHandles,
		  DWORD dwMilliseconds)
{
  /* Check inactive worker threads and terminate them. */
  if (shrink_wait_pool () == WFO_FAILED)
    return WFO_FAILED;
  /* If the number of wait objects does not exceed 64, directly call
     WaitForMultipleObjects and convert the return value. */
  if (nCount <= 64)
    {
      DWORD res = WaitForMultipleObjects (nCount, lpHandles, FALSE,
					  dwMilliseconds);
      if (res == WAIT_TIMEOUT)
	return WFO_TIMEOUT;
      else if (res >= WAIT_OBJECT_0
	       && res < WAIT_OBJECT_0 + nCount)
	return res - WAIT_OBJECT_0;
      else if (res >= WAIT_ABANDONED_0
	       && res < WAIT_ABANDONED_0 + nCount)
	return res + WFO_ABANDONED - WAIT_ABANDONED_0;
      else
	return WFO_FAILED;
    }
  /* If the wait time is 0, perform busy waiting. */
  if (dwMilliseconds == 0)
    {
      int rest = nCount % 64;
      int group = nCount / 64;
      DWORD res, count;
      for (int i = 0, offset = 0; i <= group; i++, offset += 64)
	{
	  count = (i == group) ? rest : 64;
	  /* When the number of waits is a multiple of 64, skipping the
	     last wait with a count of 0. */
	  if (count == 0)
	    break;
	  res = WaitForMultipleObjects (count, lpHandles + offset,
					FALSE, 0);
	  if (res == WAIT_TIMEOUT)
	    continue;
	  else if (res >= WAIT_OBJECT_0
		   && res < WAIT_OBJECT_0 + count)
	    return offset + res - WAIT_OBJECT_0;
	  else if (res >= WAIT_ABANDONED_0
		   && res < WAIT_ABANDONED_0 + count)
	    return offset + res + WFO_ABANDONED - WAIT_ABANDONED_0;
	  else
	    return WFO_FAILED;
	}
      return WFO_TIMEOUT;
    }
  /* If the number of objects is greater than 64 and the wait time is
     not 0, use multithreaded waiting. */
  if (start_wait_objects (&wait_info, nCount, lpHandles) == WFO_FAILED)
    return WFO_FAILED;
  DWORD res = WaitForMultipleObjects (wait_info.nh, wait_info.hs,
				      FALSE, dwMilliseconds);
  /* If the main thread wait times out, call WaitForMultipleObjects
     again with zero time-out interval to check if any objects have
     completed. The default clock resolution of Windows is 64 Hz. If a
     time less than 15.625ms is specified, the waiting time may be
     longer than the specified time, and some objects that were not
     completed in the previous call may now be completed. */
  if (res == WAIT_TIMEOUT)
    {
      res = WaitForMultipleObjects (wait_info.nh, wait_info.hs,
				    FALSE, 0);
    }
  return end_wait_and_return (&wait_info, res);
}

/* Replacement of MsgWaitForMultipleObjects, with the bWaitAll and
   dwWakeMask parameters removed. The function's return values are as
   follows:

   [0 ~ nCount-1], the return value indicates the lpHandles array
   index of the object that satisfied the wait.

   [nCount], New input of the type QS_ALLINPUT is available in the
   thread's input queue.

   [WFO_ABANDONED ~ nCount-1 + WFO_ABANDONED], the return value minus
   WFO_ABANDONED indicates the lpHandles array index of an abandoned
   mutex object that satisfied the wait.

   [WFO_TIMEOUT], The time-out interval elapsed.

   [WFO_FAILED], The function has failed. To get extended error
   information, call GetLastError. */
static DWORD
msg_wait_for_objects (DWORD nCount, HANDLE *lpHandles,
		      DWORD dwMilliseconds)
{
  /* Check inactive worker threads and terminate them. */
  if (shrink_wait_pool () == WFO_FAILED)
    return WFO_FAILED;
  /* If the number of wait objects does not exceed 63, directly call
     MsgWaitForMultipleObjects and convert the return value. */
  if (nCount <= 63)
    {
      DWORD res = MsgWaitForMultipleObjects (nCount, lpHandles, FALSE,
					     dwMilliseconds, QS_ALLINPUT);
      if (res == WAIT_TIMEOUT)
	return WFO_TIMEOUT;
      /* The return value of MsgWaitForMultipleObjects can be
	 WAIT_OBJECT_0 + nCount, indicating that a message was
	 received. So use (<=) rather than (<) here. */
      else if (res >= WAIT_OBJECT_0
	       && res <= WAIT_OBJECT_0 + nCount)
	return res - WAIT_OBJECT_0;
      else if (res >= WAIT_ABANDONED_0
	       && res < WAIT_ABANDONED_0 + nCount)
	return res + WFO_ABANDONED - WAIT_ABANDONED_0;
      else
	return WFO_FAILED;
    }
  /* If the wait time is 0, perform busy waiting. */
  if (dwMilliseconds == 0)
    {
      int rest = nCount % 63;
      int group = nCount / 63;
      DWORD res, count;
      for (int i = 0, offset = 0; i <= group; i++, offset += 63)
	{
	  count = i == group ? rest : 63;
	  /* When the number of waits is a multiple of 63, skipping the
	     last wait with a count of 0. */
	  if (count == 0)
	    break;
	  res = MsgWaitForMultipleObjects (count, lpHandles + offset,
					   FALSE, 0, QS_ALLINPUT);
	  if (res == WAIT_TIMEOUT)
	    continue;
	  else if (res >= WAIT_OBJECT_0
		   && res < WAIT_OBJECT_0 + count)
	    return offset + res - WAIT_OBJECT_0;
	  /* When a message is received during the wait, return nCount
	     directly. This is the distinction that needs to be made
	     compared to wait_for_objects. */
	  else if (res == WAIT_OBJECT_0 + count)
	    return nCount;
	  else if (res >= WAIT_ABANDONED_0
		   && res < WAIT_ABANDONED_0 + count)
	    return offset + res + WFO_ABANDONED - WAIT_ABANDONED_0;
	  else
	    return WFO_FAILED;
	}
      return WFO_TIMEOUT;
    }
  /* If the number of objects is greater than 63 and the wait time is
     not 0, use multithreaded waiting. */
  if (start_wait_objects (&wait_info, nCount, lpHandles) == WFO_FAILED)
    return WFO_FAILED;
  DWORD res = MsgWaitForMultipleObjects (wait_info.nh, wait_info.hs,
					 FALSE, dwMilliseconds,
					 QS_ALLINPUT);
  /* If the main thread wait times out, call MsgWaitForMultipleObjects
     again with zero time-out interval to check if any objects have
     completed. */
  if (res == WAIT_TIMEOUT)
    {
      res = MsgWaitForMultipleObjects (wait_info.nh, wait_info.hs,
				       FALSE, 0, QS_ALLINPUT);
    }
  return end_wait_and_return (&wait_info, res);
}


struct test
{
    /* At most 2048 events for test. */
    HANDLE es[2048];
    /* Actual number of events. Since our implementation can only wait for
       up to 63 objects in msg_wait_for_objects, the COUNT value should
       not be greater than 2047. */
    int    cnt;
};

static struct test TP;

void cleanup(void)
{
    for (int i = 0; i < TP.cnt; i++)
        CloseHandle(TP.es[i]);
    free_wait_pool();
}

#define set_event(i, place)						\
  do {									\
    if (!SetEvent (TP.es[i])) {	    				        \
      printf ("line: %d, set event fail with %d",			\
	      place, GetLastError ());					\
      cleanup ();							\
      exit (2);								\
    }									\
  } while (0)
#define dbg_set_event(i) set_event(i, __LINE__)

#define reset_event(i, place)						\
  do {									\
    if (!ResetEvent(TP.es[i])) {					\
      printf ("line: %d, reset event fail with %d",			\
	      place, GetLastError ());					\
      cleanup ();							\
      exit(2);								\
    }									\
  } while (0)
#define dbg_reset_event(i) reset_event(i, __LINE__)

int main(int argc, char *argv[])
{
    printf ("WFO_ABANDONED: %d\n", 0x10000);
    printf ("WFO_TIMEOUT  : %d\n", 0x20002);
    printf ("WFO_FAILED   : %d\n", 0xfffff);
    TP.cnt = 2047;
    int test_1 = 1;
    int test_2 = 1;
    int test_3 = 1;
    int test_time_1 = 1;
    int test_time_2 = 0;
    DWORD result;
    /* Initialize events. */
    for (int i = 0; i < TP.cnt; i++)
      {
        TP.es[i] = CreateEvent (NULL, TRUE, FALSE, NULL);
        if (!TP.es[i])
        {
            printf ("Create Event failed %d\n", GetLastError ());
            cleanup ();
            exit (1);
        }
      }
    /* timeout test. Test whether they return WFO_TIMEOUT correctly by
       calling the wait function on objects that haven't been signaled. */
    if (test_1)
      {
        printf ("[test] timeout\n");
        result = wait_for_objects (TP.cnt, TP.es, 10);
        if (result != WFO_TIMEOUT)
	  {
            printf("wait_for_objects: return %d, error %d\n", result, GetLastError ());
            cleanup ();
            exit (1);
	  }
        result = msg_wait_for_objects (TP.cnt, TP.es, 10);
        if (result != WFO_TIMEOUT)
	  {
            printf ("msg_wait_for_objects: return %d, error %d\n", result, GetLastError ());
            cleanup ();
            exit (1);
	  }
        printf ("success\n");
      }
    /* test wait_for_objects. */
    if (test_2)
      {
        printf ("[test] wait_for_objects\n");
	/* Set the events one by one, then call the wait function and
	   check whether the return result matches the event's index. */
        for (int i = 0; i < TP.cnt; i++)
	  {
            dbg_set_event(i);
            result = wait_for_objects (TP.cnt, TP.es, 1000);
            //printf ("%d, %d\n", i, result);
            if (result != i)
	      {
                printf ("2-1: return %d, error %d\n", result, GetLastError());
                cleanup ();
                exit (1);
	      }
            dbg_reset_event(i);
	  }
        /* Since multiple threads are used, the event object returned by
	   wait_for_objects may not necessarily be the first one that
	   was signaled. We can use the following print output to plot a
	   scatter diagram of the number of waits versus the signaled
	   event index to observe the distribution of wait_for_objects
	   when multiple events are signaled simultaneously. */
        for (int i = 0; i < TP.cnt; i++)
            dbg_set_event(i);
        for (int i = 0; i < TP.cnt; i++)
	  {
            result = wait_for_objects (TP.cnt, TP.es, 1000);
            //printf ("%d, %d\n", i, result);
            if (result >= TP.cnt)
            {
                printf ("2-2 return %d, error %d\n", result, GetLastError ());
                cleanup ();
                exit (1);
            }
            dbg_reset_event(result);
	  }
        printf ("success\n");
      }
    /* test msg_wait_for_objects. */
    if (test_3)
      {
        printf ("[test] msg_wait_for_objects\n");
        for (int i = 0; i < TP.cnt; i++)
	  {
	    dbg_set_event(i);
            result = msg_wait_for_objects (TP.cnt, TP.es, 1000);
            //printf ("%d, %d\n", i, result);
            if (result != i)
	      {
                printf ("3-1 return %d, error %d\n", result, GetLastError());
                cleanup ();
                exit (1);
	      }
            dbg_reset_event(i);
	  }
        for (int i = 0; i < TP.cnt; i++)
            dbg_set_event(i);
        for (int i = 0; i < TP.cnt; i++)
	  {
            result = msg_wait_for_objects (TP.cnt, TP.es, 1000);
            // printf ("%d, %d\n", i, result);
            if (result > TP.cnt)
	      {
                printf ("3-2 return %d, error %d\n", result, GetLastError ());
                cleanup ();
                exit (1);
	      }
            dbg_reset_event(result);
	  }
        printf("success\n");
      }
    /* test average wait time. */
    if (test_time_1)
      {
        printf ("[test] average time\n");
        LARGE_INTEGER start;
        LARGE_INTEGER end;
        double curr_time = 0.0;
        double total_time = 0.0;
	/* Randomly set an event object and collect the wait time to
	   obtain the average value. */
        for (int i = 0; i < 5000; i++)
	  {
            int idx = rand () % TP.cnt;
            dbg_set_event(idx);
            QueryPerformanceCounter (&start);
            result = wait_for_objects (TP.cnt, TP.es, 10);
            QueryPerformanceCounter (&end);
            if (result != idx)
	      {
                printf("time test return %d, error %d\n", result, GetLastError ());
                cleanup ();
                exit (1);
	      }
            dbg_reset_event(idx);
            curr_time = (end.QuadPart - start.QuadPart) / 10.0;
            total_time += curr_time;
            //printf("%lf\n", curr_time);
	  }
        printf ("avg time: %lf\n", total_time / 5000.0);
      }
    /* Count from 1 to the number of events, and track the waiting
       events when waiting for these events, where one events with
       random indices have already been signaled. Also, record the
       waiting time, and we can use this data to plot a graph showing
       the relationship between the number of waiting events and the
       waiting time. */
    if (test_time_2)
      {
        for (int cnt = 1; cnt <= TP.cnt; cnt++)
	  {
            LARGE_INTEGER start;
            LARGE_INTEGER end;
            double curr_time = 0.0;
            double total_time = 0.0;

            for (int i = 0; i < 1000; i++)
	      {
                int idx = rand () % cnt;
                dbg_set_event (idx);
                QueryPerformanceCounter (&start);
		/* If we set the waiting time here to 0, it can be used
		   to test busy-wait time, and it will not use
		   multithreading. */
                result = wait_for_objects (cnt, TP.es, 10);
                QueryPerformanceCounter (&end);
                if (result != idx)
		  {
		    printf("time test return %d, error %d\n", result, GetLastError ());
		    cleanup ();
		    exit (1);
		  }
                dbg_reset_event(idx);
                curr_time = (end.QuadPart - start.QuadPart) / 10.0;
                total_time += curr_time;
            }
	    /* Data in the format of (event count, waiting events)
	       separated by commas. */
            printf ("%d, %lf\n", cnt, total_time / 1000.0);
	  }
      }
    cleanup ();
    return 0;
}
