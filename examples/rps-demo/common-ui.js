'use strict';

const htmlToElement = html => {
    const template = document.createElement('template');
    html = html.trim(); // Never return a text node of whitespace as the result
    template.innerHTML = html;
    return template.content.firstChild;
}

const htmlToElements = html => {
    const template = document.createElement('template');
    template.innerHTML = html;
    return template.content.childNodes;
}

const setNodeBySelector = (selector, content) => {
    const node = document.querySelector(selector);
    if (node) {
        node.innerHTML = '';
        node.appendChild(content);
    }
}

const emptyNode = () => document.createTextNode("");

const simpleNumberRegex = "[0-9]+([.][0-9]+)?|[.][0-9]+";
const addressRegex = "0x[0-9A-Fa-f]{40}";

const shorten0x = string0x =>
    string0x.length < 11 ? string0x : `${string0x.slice(0, 6)}…${string0x.slice(-4)}`;

const renderTransaction = txHash =>
      txHash ? `<a href="${config.txExplorerUrl}${txHash}">${shorten0x(txHash)}</a>` : "unknown";
const renderAddress = address =>
      address ? address == zeroAddress ? "anyone" :
      `<a href="${config.addressExplorerUrl}${address}">${shorten0x(address)}</a>` : "unknown";
const renderWei = amountInWei =>
      `${weiToEth(amountInWei)} ETH`;

const renderConfig = () => `Running on <a href="${config.networkUrl}">${config.networkName}</a> \
with ${config.confirmationsWantedInBlocks}-block confirmations (${config.confirmationsString})
and ${config.timeoutInBlocks}-block timeouts (${config.timeoutString}). \
Your address is: <a href="${config.addressExplorerUrl}${userAddress}">${userAddress}</a>`;

// TODO: add cases for I/me/my/mine, you/you/your/yours, we/us/our/ours, they/them/their/theirs ?
// Have keyword parameters for that?
const pronoun = (who, you, lowercase) => {
    const capitalized = who == you ? "You" : "They";
    return lowercase ? capitalized.toLowerCase() : capitalized; }

