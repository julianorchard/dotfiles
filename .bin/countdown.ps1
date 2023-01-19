##   Countdown.ps1  ---  Send emails to someone every x seconds...

# Copyright (c) 2022   Julian Orchard <jorchard@pm.me>

## Description:

# Send annoying reminder emails every X until X (counting down until then).

## License:

# See /LICENSE file in the root of this repository.

## Code:

# * TODO: Could/should use [mandatory]?
#   SCHEDULED: <2022-12-19>
#   Julian <jorchard@pm.me>
Param (
    $EndTime,
    $Interval,
    $Recipient,
    $Description
)

Function Send-Outlook-Email {
    Param (
        $To,
        $Subject,
        $Body
    )

    $outlook = New-Object -ComObject Outlook.Application
    $outlookMail = $outlook.CreateItem(0)

    $outlookMail.To = "$To"
    $outlookMail.Subject = "$Subject"
    $outlookMail.Body = "$Body"
    $outlookMail.Send()
    $timestamp = Get-Date -Format HH:mm
    Write-Host "Sent email to $To at $timestamp."
    Start-Sleep 2
}

Function Script-Usage {
    Write-Host "This script requires the following arguments:"
    Write-Host "  -EndTime     =>  Time till countdown end"
    Write-Host "  -Interval    =>  Countdown interval (seconds)"
    Write-Host "  -Recipient   =>  Recipient(s), semi-colon for multiple`r`n"
    Write-Host "Optional:"
    Write-Host "  -Description =>  Event description string"
    Exit
}

#########################

If (-Not ($EndTime))   { Throw "`r`nError: EndTime is required`r`n" }
If (-Not ($Interval))  { Throw "`r`nError: Interval is required`r`n" }
If (-Not ($Recipient)) { Throw "`r`nError: Recipient is required`r`n" }

If ($Description -ne "")
{
    $Description = ": $Description"
    $descriptionBody = "This reminder is about$Description"
}

$endTimeFormatted = (Get-Date $EndTime -Format HH:mm:ss)

While ($true)
{
    $currentTime = Get-Date -Format HH:mm:ss

    $timeRemaining = [math]::Round((New-TimeSpan -Start $currentTime -End $endTimeFormatted).TotalMinutes)
    $timeRemaining = "$timeRemaining minutes to go"

    $subject = "Upcoming ($timeRemaining) Reminder$Description"
    $body = @"
Hello there,

You have $timeRemaining! $descriptionBody

This message is due to send every $Interval seconds...

Kind regards,
"@

    # Mail Sending Call
    Try
    {
        Send-Outlook-Email -To $Recipient -Subject $subject -Body $body
    }
    Catch
    {
        Write-Host "Unable to send email (for some reason), to $Recipient. Please try again."
        Exit
    }
    If ((Get-Date) -gt (Get-Date $EndTime))
    {
        Write-Host "Script has finished notifying its victi- ...recipient!"
        Break
    }
    Else
    {
        Start-Sleep -Seconds $Interval
    }
}
