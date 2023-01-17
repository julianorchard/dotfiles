;; Ctrl + Start + l
$^#l::
{
  SetStoreCapsLockMode 0
  a := "{NumLock}"
  b := "{CapsLock}"
  c := "{ScrollLock}"
  SetNumLockState(0)
  SetCapsLockState(0)
  SetScrollLockState(0)
  ql := 1000
  Loop(8)
  {
    Send(a)
    Sleep(ql)
    Send(a)
    Send(b)
    Sleep(ql)
    Send(b)
    Send(c)
    Sleep(ql)
    Send(c)
    ql := ql/2
  }
  Sleep(200)
  Send(a)
  Send(b)
  Send(c)
  Sleep(50)
  Send(a)
  Send(b)
  Send(c)
  Sleep(100)
  Send(a)
  Send(b)
  Send(c)
  Sleep(50)
  Send(a)
  Send(b)
  Send(c)
}

;; Ctrl + Start + w
$^#w::
{
  SetStoreCapsLockMode 0
  a := "{NumLock}"
  b := "{CapsLock}"
  c := "{ScrollLock}"
  SetNumLockState(0)
  SetCapsLockState(0)
  SetScrollLockState(0)
  count := 1
  Loop(3)
  {
    Loop(3)
    {
      Sleep(100)
      Send(c)
      Sleep(60)
      Send(c)
      Send(b)
      Sleep(60)
      Send(b)
      Send(a)
      Sleep(60)
      Send(a)
    }
    if (count == 1)
    {
      Sleep(200)
      Send(c)
      Sleep(200)
      Send(c)
      Send(b)
      Sleep(200)
      Send(b)
    }
    else if (count == 2)
    {
      Sleep(200)
      Send(b)
      Sleep(200)
      Send(b)
      Send(a)
      Sleep(200)
      Send(a)
    }
    else
    {
      Sleep(200)
      Send(a)
      Sleep(200)
      Send(a)
      Send(b)
      Sleep(200)
      Send(b)
      Send(a)
      Sleep(100)
      Send(b)
      Sleep(100)
      Send(c)
      Sleep(800)
      Send(c)
      Sleep(200)
      Send(b)
      Sleep(200)
      Send(a)
      Sleep(400)
      Send(a)
      Send(b)
      Send(c)
      Sleep(400)
      Send(a)
      Send(b)
      Send(c)
    }
    count := count+1
  }
}
