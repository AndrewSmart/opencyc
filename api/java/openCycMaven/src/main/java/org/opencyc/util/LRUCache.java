/* $Id: LRUCache.java 135739 2011-09-09 14:24:55Z bbouldin $
 *
 * LRU (Least Recently Used) Cache implementation.
 *
 * @author Tony Brusseau
 *
 * <p>Copyright 2010 Cycorp, Inc., license is open source GNU LGPL.
 * <p><a href="http://www.opencyc.org/license.txt">the license</a>
 * <p><a href="http://www.opencyc.org">www.opencyc.org</a>
 * <p><a href="http://www.sourceforge.net/projects/opencyc">OpenCyc at SourceForge</a>
 * <p>
 * THIS SOFTWARE AND KNOWLEDGE BASE CONTENT ARE PROVIDED ``AS IS'' AND
 * ANY EXPRESSED OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO,
 * THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
 * PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE OPENCYC
 * ORGANIZATION OR ITS CONTRIBUTORS BE LIABLE FOR ANY DIRECT,
 * INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
 * (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
 * SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
 * STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
 * ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE AND KNOWLEDGE
 * BASE CONTENT, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 */

package org.opencyc.util;

//// Internal Imports

import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

//// External Imports

/**
 * <P>
 * LRUCache is designed to...
 * 
 * <P>
 * Copyright (c) 2008 Cycorp, Inc. All rights reserved. <BR>
 * This software is the proprietary information of Cycorp, Inc.
 * <P>
 * Use is subject to license terms.
 * 
 * Created on : Feb 28, 2010, 3:27:51 PM Author : tbrussea
 * 
 * @version $Id: LRUCache.java 135739 2011-09-09 14:24:55Z bbouldin $
 */
public class LRUCache<K, V> implements Map<K, V> {

	// // Constructors

	/** Creates a new synchronized instance of LRUCache. */
	public LRUCache(int defaultSize, int maxSize) {
		this(defaultSize, maxSize, true);
	}

	/** Creates a new instance of LRUCache. */
	public LRUCache(int defaultSize, int maxSize, boolean isSynchronized) {
		this(defaultSize, maxSize, isSynchronized, .75f);
	}

	/** Creates a new instance of LRUCache. */
	public LRUCache(int defaultSize, final int maxSize, boolean isSynchronized,
			float loadFactor) {
		this.maxSize = maxSize;
		this.cache = new LinkedHashMap<K, V>(defaultSize, loadFactor, true) {
			private static final long serialVersionUID = 7046745637375687927L;

			@SuppressWarnings("rawtypes")
			@Override
			protected boolean removeEldestEntry(Map.Entry<K, V> eldest) {
				V val = eldest.getValue();
				if (val instanceof CachedValue) {
					if (((CachedValue) val).isExpired()) {
						return true;
					}
				}
				return size() > maxSize;
			}
		};
		if (isSynchronized) {
			this.cache = Collections.synchronizedMap(cache);
		}
	}

	// // Public Area
	public void clear() {
		cache.clear();
	}

	public boolean containsKey(Object key) {
		return cache.containsKey((K) key);
	}

	public boolean containsValue(Object value) {
		return cache.containsValue((V) value);
	}

	public Set<Map.Entry<K, V>> entrySet() {
		return cache.entrySet();
	}

	public V get(Object key) {
		return cache.get((K) key);
	}

	public boolean isEmpty() {
		return cache.isEmpty();
	}

	public Set<K> keySet() {
		return cache.keySet();
	}

	public V put(K key, V value) {
		return cache.put(key, value);
	}

	public void putAll(Map<? extends K, ? extends V> m) {
		cache.putAll(m);
	}

	public V remove(Object key) {
		return cache.remove((K) key);
	}

	public int size() {
		return cache.size();
	}

	public Collection<V> values() {
		return cache.values();
	}

	// // Protected Area

	// // Private Area

	// // Internal Rep

	private int maxSize;
	private Map<K, V> cache;

	// // Main

}
