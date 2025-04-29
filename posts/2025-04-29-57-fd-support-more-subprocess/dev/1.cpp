#include <Windows.h>

#define WFO_ABANDONED 0x10000
#define WFO_TIMEOUT   0x20002
#define WFO_FAILED    0xfffff
#define WFO_MAX_WAIT  2048

static DWORD WFMO(int nCount, HANDLE* lpHandles, BOOL bWaitAll, DWORD dwMilliseconds)
{
    static LONG index = 0;
    static PTP_WAIT waits[WFO_MAX_WAIT];
    static HANDLE hevent;
    hevent = CreateEvent(NULL, FALSE, FALSE, NULL);
    if (hevent == NULL)
        return WFO_FAILED;
    index = -1;
    for (int i = 0; i < nCount; i++)
    {
        auto wait = CreateThreadpoolWait(
            [](auto, auto id, auto, auto)
            {
                LONG res = InterlockedCompareExchange(&index, (LONG)id, -1);
                if (res == -1)
                    SetEvent(hevent);
            }, (PVOID)i, NULL);
        SetThreadpoolWait(wait, lpHandles[i], NULL);
        waits[i] = wait;
    }
    DWORD result = WaitForSingleObject(hevent, dwMilliseconds);
    for (int i = 0; i < nCount; i++)
        CloseThreadpoolWait(waits[i]);
    CloseHandle(hevent);
    switch (result)
    {
    case WAIT_OBJECT_0:
        return index;
    case WAIT_TIMEOUT:
        return WFO_TIMEOUT;
    case WAIT_ABANDONED:
    case WAIT_FAILED:
    default:
        return WFO_FAILED;
    }
}

int main()
{
  
}
