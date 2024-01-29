#!/usr/bin/env sh

set -euf

# 1. Check if there's any new entries in LogSeq for the day
# 2. Send a reminder (notify-send, ntfy) if there are none
# 3. Run on a cron every lunchtime or evening or something idk
#    - This is the one I went with in the end:
#      0 13 * * 1-5 export DISPLAY=:0.0 && /home/julian/.bin/logseq-reminder.sh
#    - We need the `export DISPLAY` for the `notify-send` notification
# 4. Profit???

JOURNAL_PATH="${HOME}/Documents/logseq/journals"
JOURNAL_FILE=$(date "+%Y_%m_%d.md")

rng_msg() {
    # this was a stupid way of doing this lmao
    msg="Logseq_now_please Fill_in_logseq_you_fuckwit Logseq_is_love,_logseq_is_life"

    lower=1
    upper=3
    range=$((upper-lower+1))
    rando=$$
    number=$(($((rando % range)) + lower))

    echo "$msg" | awk -v n="${number}" '{print $n}' | sed 's/_/ /g'
}

[ ! -f "${JOURNAL_PATH}/${JOURNAL_FILE}" ] \
    && message=$(rng_msg) \
    && "${HOME}/.bin/serious-notification.sh" "${message}"
