// Claude Verify - Popup Script

// State
let state = {
  project: null,
  knownIssues: [],
  daemonConnected: false,
  daemonUrl: 'http://localhost:9847'
};

// DOM elements
const elements = {
  status: document.getElementById('status'),
  projectInfo: document.getElementById('project-info'),
  loadContext: document.getElementById('load-context'),
  fileInput: document.getElementById('file-input'),
  knownIssues: document.getElementById('known-issues'),
  issueCount: document.getElementById('issue-count'),
  injectContext: document.getElementById('inject-context'),
  verifyCode: document.getElementById('verify-code'),
  recordFeedback: document.getElementById('record-feedback'),
  daemonIndicator: document.getElementById('daemon-indicator'),
  daemonText: document.getElementById('daemon-text'),
  daemonUrl: document.getElementById('daemon-url'),
  connectDaemon: document.getElementById('connect-daemon'),
  feedbackModal: document.getElementById('feedback-modal'),
  feedbackForm: document.getElementById('feedback-form'),
  cancelFeedback: document.getElementById('cancel-feedback')
};

// Initialize
document.addEventListener('DOMContentLoaded', async () => {
  await loadState();
  setupEventListeners();
  updateUI();
  checkDaemonConnection();
});

// Load saved state from storage
async function loadState() {
  try {
    const stored = await chrome.storage.local.get(['project', 'knownIssues', 'daemonUrl']);
    if (stored.project) state.project = stored.project;
    if (stored.knownIssues) state.knownIssues = stored.knownIssues;
    if (stored.daemonUrl) {
      state.daemonUrl = stored.daemonUrl;
      elements.daemonUrl.value = stored.daemonUrl;
    }
  } catch (e) {
    console.error('Failed to load state:', e);
  }
}

// Save state to storage
async function saveState() {
  try {
    await chrome.storage.local.set({
      project: state.project,
      knownIssues: state.knownIssues,
      daemonUrl: state.daemonUrl
    });
  } catch (e) {
    console.error('Failed to save state:', e);
  }
}

// Setup event listeners
function setupEventListeners() {
  elements.loadContext.addEventListener('click', () => elements.fileInput.click());
  elements.fileInput.addEventListener('change', handleFileSelect);
  elements.injectContext.addEventListener('click', injectContext);
  elements.verifyCode.addEventListener('click', verifySelectedCode);
  elements.recordFeedback.addEventListener('click', showFeedbackModal);
  elements.connectDaemon.addEventListener('click', connectToDaemon);
  elements.feedbackForm.addEventListener('submit', handleFeedbackSubmit);
  elements.cancelFeedback.addEventListener('click', hideFeedbackModal);
}

// Handle file selection
async function handleFileSelect(event) {
  const file = event.target.files[0];
  if (!file) return;

  try {
    const content = await file.text();
    const parsed = parseTOML(content);

    state.project = {
      name: parsed.project?.name || 'Unknown',
      languages: parsed.project?.languages || [],
      safetyLevel: parsed.project?.safety_level || 'standard'
    };

    state.knownIssues = parsed.known_issues?.items || [];

    await saveState();
    updateUI();

    // Notify content script
    const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
    if (tab?.id) {
      chrome.tabs.sendMessage(tab.id, {
        type: 'PROJECT_LOADED',
        project: state.project,
        knownIssues: state.knownIssues
      });
    }
  } catch (e) {
    console.error('Failed to parse config:', e);
    alert('Failed to parse .claude-context.toml: ' + e.message);
  }
}

// Simple TOML parser (handles our specific format)
function parseTOML(content) {
  const result = {};
  let currentSection = null;
  let currentKey = null;
  let inArray = false;
  let arrayContent = [];

  const lines = content.split('\n');

  for (const line of lines) {
    const trimmed = line.trim();

    // Skip comments and empty lines
    if (!trimmed || trimmed.startsWith('#')) continue;

    // Section header
    const sectionMatch = trimmed.match(/^\[([^\]]+)\]$/);
    if (sectionMatch) {
      if (inArray && currentKey) {
        setNestedValue(result, currentSection, currentKey, arrayContent);
        arrayContent = [];
        inArray = false;
      }
      currentSection = sectionMatch[1];
      if (!result[currentSection]) result[currentSection] = {};
      continue;
    }

    // Key-value pair
    const kvMatch = trimmed.match(/^(\w+)\s*=\s*(.+)$/);
    if (kvMatch) {
      if (inArray && currentKey) {
        setNestedValue(result, currentSection, currentKey, arrayContent);
        arrayContent = [];
        inArray = false;
      }

      const [, key, value] = kvMatch;
      currentKey = key;

      // Check if value is start of array
      if (value.trim() === '[') {
        inArray = true;
        arrayContent = [];
      } else if (value.trim().startsWith('[') && value.trim().endsWith(']')) {
        // Inline array
        const arrContent = value.trim().slice(1, -1);
        const items = parseInlineArray(arrContent);
        setNestedValue(result, currentSection, key, items);
      } else {
        // Simple value
        setNestedValue(result, currentSection, key, parseValue(value));
      }
      continue;
    }

    // Array content
    if (inArray) {
      if (trimmed === ']') {
        setNestedValue(result, currentSection, currentKey, arrayContent);
        arrayContent = [];
        inArray = false;
      } else {
        const value = parseValue(trimmed.replace(/,$/, ''));
        if (value !== null && value !== '') {
          arrayContent.push(value);
        }
      }
    }
  }

  // Handle unclosed array
  if (inArray && currentKey) {
    setNestedValue(result, currentSection, currentKey, arrayContent);
  }

  return result;
}

function setNestedValue(obj, section, key, value) {
  if (section) {
    const parts = section.split('.');
    let current = obj;
    for (const part of parts) {
      if (!current[part]) current[part] = {};
      current = current[part];
    }
    current[key] = value;
  } else {
    obj[key] = value;
  }
}

function parseValue(str) {
  const trimmed = str.trim();

  // String
  if ((trimmed.startsWith('"') && trimmed.endsWith('"')) ||
      (trimmed.startsWith("'") && trimmed.endsWith("'"))) {
    return trimmed.slice(1, -1);
  }

  // Boolean
  if (trimmed === 'true') return true;
  if (trimmed === 'false') return false;

  // Number
  if (/^-?\d+(\.\d+)?$/.test(trimmed)) {
    return parseFloat(trimmed);
  }

  return trimmed;
}

function parseInlineArray(content) {
  const items = [];
  let current = '';
  let inString = false;
  let stringChar = '';

  for (const char of content) {
    if (!inString && (char === '"' || char === "'")) {
      inString = true;
      stringChar = char;
      current += char;
    } else if (inString && char === stringChar) {
      inString = false;
      current += char;
    } else if (!inString && char === ',') {
      const value = parseValue(current.trim());
      if (value !== null && value !== '') items.push(value);
      current = '';
    } else {
      current += char;
    }
  }

  if (current.trim()) {
    const value = parseValue(current.trim());
    if (value !== null && value !== '') items.push(value);
  }

  return items;
}

// Update UI based on state
function updateUI() {
  // Project info
  if (state.project) {
    elements.projectInfo.innerHTML = `
      <div class="project-name">${escapeHtml(state.project.name)}</div>
      <div class="project-langs">Languages: ${state.project.languages.join(', ') || 'none'}</div>
      <div class="project-level">Safety: ${state.project.safetyLevel}</div>
    `;
    elements.injectContext.disabled = false;
    elements.status.textContent = 'Ready';
    elements.status.classList.add('connected');
  } else {
    elements.projectInfo.innerHTML = '<p class="no-project">No project loaded</p>';
    elements.injectContext.disabled = true;
    elements.status.textContent = 'No Project';
    elements.status.classList.remove('connected');
  }

  // Known issues
  elements.issueCount.textContent = `(${state.knownIssues.length})`;
  if (state.knownIssues.length > 0) {
    elements.knownIssues.innerHTML = state.knownIssues.map(issue =>
      `<li>${escapeHtml(issue)}</li>`
    ).join('');
  } else {
    elements.knownIssues.innerHTML = '<li class="no-issues">No known issues</li>';
  }

  // Daemon status
  if (state.daemonConnected) {
    elements.daemonIndicator.classList.remove('offline');
    elements.daemonIndicator.classList.add('online');
    elements.daemonText.textContent = 'Connected';
    elements.verifyCode.disabled = false;
  } else {
    elements.daemonIndicator.classList.remove('online');
    elements.daemonIndicator.classList.add('offline');
    elements.daemonText.textContent = 'Not connected';
    elements.verifyCode.disabled = true;
  }
}

// Inject context into Claude chat
async function injectContext() {
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
  if (!tab?.id) return;

  // Build context message
  let contextMessage = `## Project Context: ${state.project.name}\n\n`;
  contextMessage += `**Languages:** ${state.project.languages.join(', ')}\n`;
  contextMessage += `**Safety Level:** ${state.project.safetyLevel}\n\n`;

  if (state.knownIssues.length > 0) {
    contextMessage += `### Known Issues to Avoid\n\n`;
    contextMessage += state.knownIssues.map(issue => `- ${issue}`).join('\n');
    contextMessage += '\n\n';
  }

  contextMessage += `*Context injected by claude-verify*`;

  chrome.tabs.sendMessage(tab.id, {
    type: 'INJECT_CONTEXT',
    context: contextMessage
  });
}

// Verify selected code
async function verifySelectedCode() {
  const [tab] = await chrome.tabs.query({ active: true, currentWindow: true });
  if (!tab?.id) return;

  chrome.tabs.sendMessage(tab.id, { type: 'GET_SELECTED_CODE' }, async (response) => {
    if (response?.code) {
      try {
        const result = await fetch(`${state.daemonUrl}/api/verify`, {
          method: 'POST',
          headers: { 'Content-Type': 'application/json' },
          body: JSON.stringify({
            code: response.code,
            language: response.language || 'rust',
            project: state.project?.name
          })
        });

        const data = await result.json();

        chrome.tabs.sendMessage(tab.id, {
          type: 'SHOW_VERIFICATION_RESULT',
          result: data
        });
      } catch (e) {
        console.error('Verification failed:', e);
        alert('Verification failed: ' + e.message);
      }
    }
  });
}

// Check daemon connection
async function checkDaemonConnection() {
  try {
    const response = await fetch(`${state.daemonUrl}/api/health`, {
      method: 'GET',
      signal: AbortSignal.timeout(2000)
    });
    state.daemonConnected = response.ok;
  } catch (e) {
    state.daemonConnected = false;
  }
  updateUI();
}

// Connect to daemon
async function connectToDaemon() {
  state.daemonUrl = elements.daemonUrl.value.trim();
  await saveState();
  await checkDaemonConnection();

  if (state.daemonConnected) {
    alert('Connected to verification daemon!');
  } else {
    alert('Could not connect to daemon at ' + state.daemonUrl);
  }
}

// Show feedback modal
function showFeedbackModal() {
  elements.feedbackModal.classList.remove('hidden');
}

// Hide feedback modal
function hideFeedbackModal() {
  elements.feedbackModal.classList.add('hidden');
  elements.feedbackForm.reset();
}

// Handle feedback submission
async function handleFeedbackSubmit(event) {
  event.preventDefault();

  const feedback = {
    output: document.getElementById('fb-output').value,
    issue: document.getElementById('fb-issue').value,
    correction: document.getElementById('fb-correction').value,
    category: document.getElementById('fb-category').value,
    project: state.project?.name,
    timestamp: new Date().toISOString()
  };

  // Store locally
  const stored = await chrome.storage.local.get(['feedbackItems']);
  const items = stored.feedbackItems || [];
  items.push(feedback);
  await chrome.storage.local.set({ feedbackItems: items });

  // Send to daemon if connected
  if (state.daemonConnected) {
    try {
      await fetch(`${state.daemonUrl}/api/feedback`, {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify(feedback)
      });
    } catch (e) {
      console.error('Failed to send feedback to daemon:', e);
    }
  }

  // Add to known issues
  state.knownIssues.push(feedback.issue);
  await saveState();
  updateUI();

  hideFeedbackModal();
  alert('Feedback recorded!');
}

// Escape HTML
function escapeHtml(str) {
  const div = document.createElement('div');
  div.textContent = str;
  return div.innerHTML;
}
