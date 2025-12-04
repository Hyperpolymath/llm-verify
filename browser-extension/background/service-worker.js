// Claude Verify - Background Service Worker

// Configuration
const DEFAULT_DAEMON_URL = 'http://localhost:9847';

// State
let daemonConnected = false;

// Initialize
console.log('[Claude Verify] Background service worker started');

// Check daemon health periodically
setInterval(checkDaemonHealth, 30000);
checkDaemonHealth();

// Listen for messages
chrome.runtime.onMessage.addListener((message, sender, sendResponse) => {
  switch (message.type) {
    case 'CHECK_DAEMON':
      checkDaemonHealth().then(connected => {
        sendResponse({ connected });
      });
      return true;

    case 'VERIFY_CODE':
      verifyCode(message.code, message.language, message.project)
        .then(result => sendResponse({ result }))
        .catch(error => sendResponse({ error: error.message }));
      return true;

    case 'SUBMIT_FEEDBACK':
      submitFeedback(message.feedback)
        .then(() => sendResponse({ success: true }))
        .catch(error => sendResponse({ error: error.message }));
      return true;
  }
});

// Check if daemon is running
async function checkDaemonHealth() {
  try {
    const { daemonUrl } = await chrome.storage.local.get(['daemonUrl']);
    const url = daemonUrl || DEFAULT_DAEMON_URL;

    const response = await fetch(`${url}/api/health`, {
      method: 'GET',
      signal: AbortSignal.timeout(2000)
    });

    daemonConnected = response.ok;

    // Update badge
    if (daemonConnected) {
      chrome.action.setBadgeText({ text: '' });
      chrome.action.setBadgeBackgroundColor({ color: '#0a0' });
    } else {
      chrome.action.setBadgeText({ text: '!' });
      chrome.action.setBadgeBackgroundColor({ color: '#c00' });
    }

    return daemonConnected;
  } catch (e) {
    daemonConnected = false;
    chrome.action.setBadgeText({ text: '!' });
    chrome.action.setBadgeBackgroundColor({ color: '#c00' });
    return false;
  }
}

// Verify code with daemon
async function verifyCode(code, language, project) {
  const { daemonUrl } = await chrome.storage.local.get(['daemonUrl']);
  const url = daemonUrl || DEFAULT_DAEMON_URL;

  const response = await fetch(`${url}/api/verify`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify({ code, language, project })
  });

  if (!response.ok) {
    throw new Error(`Verification failed: ${response.statusText}`);
  }

  return await response.json();
}

// Submit feedback to daemon
async function submitFeedback(feedback) {
  const { daemonUrl } = await chrome.storage.local.get(['daemonUrl']);
  const url = daemonUrl || DEFAULT_DAEMON_URL;

  const response = await fetch(`${url}/api/feedback`, {
    method: 'POST',
    headers: { 'Content-Type': 'application/json' },
    body: JSON.stringify(feedback)
  });

  if (!response.ok) {
    throw new Error(`Feedback submission failed: ${response.statusText}`);
  }
}

// Handle installation
chrome.runtime.onInstalled.addListener((details) => {
  if (details.reason === 'install') {
    console.log('[Claude Verify] Extension installed');

    // Set default settings
    chrome.storage.local.set({
      daemonUrl: DEFAULT_DAEMON_URL,
      feedbackItems: [],
      project: null,
      knownIssues: []
    });
  }
});
