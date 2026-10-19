/* Small Windows boundary. No application state or music policy lives here. */
#define UNICODE
#define _UNICODE
#define WIN32_LEAN_AND_MEAN
#include <windows.h>
#include <shlobj.h>
#include <commdlg.h>
#include <wchar.h>

/* Hybrid GPU drivers inspect these exports in the EXE before creating an
 * OpenGL context. Request the high-performance GPU when available; explicit
 * OS/user and device power policies still take precedence. No registry edits.
 * https://www.glfw.org/docs/latest/compile.html#compile_options_win32 */
__declspec(dllexport) DWORD NvOptimusEnablement = 1;
__declspec(dllexport) DWORD AmdPowerXpressRequestHighPerformance = 1;

static HANDLE instance_mutex = NULL;
static HANDLE activation_event = NULL;

int sr_begin(void) {
    instance_mutex = CreateMutexW(NULL, FALSE, L"Local\\Stillroom_Instance_v1");
    DWORD state = GetLastError();
    if (!instance_mutex) return -1;
    activation_event = CreateEventW(NULL, FALSE, FALSE, L"Local\\Stillroom_Activate_v1");
    if (!activation_event) { CloseHandle(instance_mutex); instance_mutex=NULL; return -1; }
    if (state == ERROR_ALREADY_EXISTS) {
        SetEvent(activation_event);
        HWND window = FindWindowW(NULL, L"Stillroom");
        if (window) { ShowWindow(window, SW_RESTORE); SetForegroundWindow(window); }
        CloseHandle(instance_mutex); instance_mutex=NULL;
        CloseHandle(activation_event); activation_event=NULL;
        return 0;
    }
    return 1;
}
int sr_activated(void) { return activation_event && WaitForSingleObject(activation_event,0)==WAIT_OBJECT_0; }
void sr_end(void) {
    SetThreadExecutionState(ES_CONTINUOUS);
    if (activation_event) CloseHandle(activation_event);
    if (instance_mutex) CloseHandle(instance_mutex);
    activation_event=NULL; instance_mutex=NULL;
}
void sr_power(int playing, int fullscreen) {
    SetThreadExecutionState(ES_CONTINUOUS | (playing ? ES_SYSTEM_REQUIRED : 0) |
                            (playing && fullscreen ? ES_DISPLAY_REQUIRED : 0));
}
int sr_japanese(void) { return PRIMARYLANGID(GetUserDefaultUILanguage()) == LANG_JAPANESE; }

typedef struct { int wanted, count, found; RECT bounds; int primary; WCHAR name[32]; } MonitorQuery;
static BOOL CALLBACK monitor_cb(HMONITOR monitor, HDC dc, LPRECT rect, LPARAM parameter) {
    (void)dc; (void)rect;
    MonitorQuery *query=(MonitorQuery*)parameter;
    if(query->count++ == query->wanted) {
        MONITORINFOEXW info; ZeroMemory(&info,sizeof(info)); info.cbSize=sizeof(info);
        if(GetMonitorInfoW(monitor,(MONITORINFO*)&info)) {
            query->found=1; query->bounds=info.rcMonitor;
            query->primary=(info.dwFlags & MONITORINFOF_PRIMARY)!=0;
            lstrcpynW(query->name,info.szDevice,32);
        }
    }
    return TRUE;
}
int sr_monitors(void) {
    MonitorQuery query; ZeroMemory(&query,sizeof(query)); query.wanted=-1;
    EnumDisplayMonitors(NULL,NULL,monitor_cb,(LPARAM)&query); return query.count;
}
int sr_monitor(int index, int *values, WCHAR *name, int capacity) {
    MonitorQuery query; ZeroMemory(&query,sizeof(query)); query.wanted=index;
    EnumDisplayMonitors(NULL,NULL,monitor_cb,(LPARAM)&query);
    if(!query.found) return 0;
    values[0]=query.bounds.left; values[1]=query.bounds.top;
    values[2]=query.bounds.right-query.bounds.left;
    values[3]=query.bounds.bottom-query.bounds.top; values[4]=query.primary;
    lstrcpynW(name,query.name,capacity); return 1;
}
int sr_position(HWND window,int x,int y,int width,int height) {
    /* The GL surface is one pixel larger on every edge. Clip the overscan to
       this display so it never covers neighboring monitors. Windows owns a
       successfully assigned region; only failed assignments are freed here. */
    HRGN clip=CreateRectRgn(1,1,width-1,height-1);
    if(!clip) return 0;
    if(!SetWindowRgn(window,clip,FALSE)) { DeleteObject(clip); return 0; }
    return SetWindowPos(window,NULL,x,y,width,height,SWP_NOZORDER|SWP_NOACTIVATE)!=0;
}
int sr_unclip(HWND window) { return SetWindowRgn(window,NULL,TRUE)!=0; }
int sr_borderless_matches(HWND window,int x,int y,int width,int height) {
    RECT outer,visible;
    HRGN clip=CreateRectRgn(0,0,0,0);
    if(!clip) return 0;
    int kind=GetWindowRgn(window,clip);
    int ok=GetWindowRect(window,&outer) && kind==SIMPLEREGION;
    if(ok) {
        GetRgnBox(clip,&visible);
        ok=outer.left==x-1 && outer.top==y-1 &&
           outer.right==x+width+1 && outer.bottom==y+height+1 &&
           visible.left==1 && visible.top==1 &&
           visible.right==width+1 && visible.bottom==height+1;
    }
    DeleteObject(clip); return ok;
}
int sr_is_unclipped(HWND window) {
    HRGN clip=CreateRectRgn(0,0,0,0);
    if(!clip) return 0;
    int kind=GetWindowRgn(window,clip);
    DeleteObject(clip); return IsWindow(window) && kind==ERROR;
}
static int CALLBACK folder_cb(HWND dialog,UINT message,LPARAM value,LPARAM initial) {
    (void)value;
    if(message==BFFM_INITIALIZED && initial && *((const WCHAR*)initial))
        SendMessageW(dialog,BFFM_SETSELECTIONW,TRUE,initial);
    return 0;
}
int sr_pick_folder(HWND owner,const WCHAR *initial,WCHAR *result,int capacity) {
    HRESULT initialized=OleInitialize(NULL);
    if(FAILED(initialized)) return -1;
    BROWSEINFOW info; ZeroMemory(&info,sizeof(info));
    info.hwndOwner=owner; info.lpszTitle=L"Choose your music folder";
    info.ulFlags=BIF_RETURNONLYFSDIRS|BIF_USENEWUI|BIF_NONEWFOLDERBUTTON;
    info.lpfn=folder_cb; info.lParam=(LPARAM)initial;
    PIDLIST_ABSOLUTE chosen=SHBrowseForFolderW(&info);
    int status=0;
    if(chosen) {
        status=SHGetPathFromIDListEx(chosen,result,(DWORD)capacity,0) ? 1 : -1;
        CoTaskMemFree(chosen);
    }
    OleUninitialize(); return status;
}
void sr_message(const WCHAR *message) { MessageBoxW(NULL,message,L"Stillroom",MB_OK|MB_ICONINFORMATION); }

int sr_pick_picture(HWND owner,WCHAR *result,int capacity) {
    OPENFILENAMEW info; ZeroMemory(&info,sizeof(info));
    result[0]=0; info.lStructSize=sizeof(info); info.hwndOwner=owner;
    info.lpstrFilter=L"Images (PNG, JPEG, BMP)\0*.png;*.jpg;*.jpeg;*.bmp\0\0";
    info.lpstrFile=result; info.nMaxFile=(DWORD)capacity;
    info.lpstrTitle=L"Add a Stillroom background";
    info.Flags=OFN_FILEMUSTEXIST|OFN_PATHMUSTEXIST|OFN_NOCHANGEDIR|OFN_EXPLORER;
    if(GetOpenFileNameW(&info)) return 1;
    return CommDlgExtendedError()==0 ? 0 : -1;
}
